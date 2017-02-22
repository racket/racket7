#lang racket/base
(require "../common/check.rkt"
         "read-and-peek.rkt"
         "input-port.rkt"
         "internal-pipe.rkt"
         (submod "bytes-input.rkt" internal)
         "../string/utf-8-decode.rkt"
         "count.rkt")

(provide read-char
         read-string
         read-string!
         
         peek-char
         peek-string
         peek-string!)

;; ----------------------------------------

;; Read up to `(- end start)` characters by UTF-8 decoding of bytes,
;; producing at least one character unless `zero-ok?`, but it's
;; possible that fewer that `(- end start)` characters are read. The
;; result is either EOF or the number of read characters
(define (read-some-chars! who orig-in str start end
                          #:zero-ok? [zero-ok? #f]
                          #:extra-bytes-amt [extra-bytes-amt 0]
                          #:keep-eof? [keep-eof? #f]
                          #:just-peek? [just-peek? #f]
                          #:skip [skip-k 0]) ; must be 0 if `(not just-peek?)`
  (define amt (- end start))
  (define bstr (make-bytes amt))
  ;; We're allowed to read up to `amt` characters, which means at
  ;; least `amt` bytes. However, if we read `amt` bytes, the last
  ;; `utf-8-max-aborts-amt` might not be ready to convert without
  ;; reading even more. If the port doesn't have more bytes available,
  ;; we'd be stuck, even though we may have enough to prdocue a
  ;; result. So, read most of the bytes, but peek the tail.
  (define min-consume-ok-amt (if just-peek?
                                 0
                                 (max 0 (- amt utf-8-max-aborts-amt))))
  (define consumed-v
    (if (positive? min-consume-ok-amt)
        (read-some-bytes! who orig-in bstr 0 min-consume-ok-amt
                          #:zero-ok? zero-ok?
                          #:copy-bstr? #f
                          #:keep-eof? keep-eof?)
        0))
  (define v
    (cond
     [(and (exact-integer? consumed-v)
           (= consumed-v min-consume-ok-amt))
      (let ([v2 (peek-some-bytes! who orig-in 
                                  bstr consumed-v (+ consumed-v (- amt min-consume-ok-amt)) skip-k
                                  #:zero-ok? #t)])
        (cond
         [(exact-integer? v2) (+ consumed-v v2)]
         [(zero? consumed-v) v2]
         [else consumed-v]))]
     [else consumed-v]))
  (cond
   [(not (exact-integer? v)) v]
   [(zero? v) 0]
   [else
    (define-values (used-bytes got-chars state)
      (utf-8-decode! bstr 0 v
                     str start amt
                     #:error-char #\?
                     #:abort-mode 'state))
    ;; The `state` result can't be 'continues, because N
    ;; bytes will never produce > N chars; it can't be
    ;; 'error, because we provide an error character; it
    ;; can't be 'aborts, because we request an abort state
    (cond
     [(zero? got-chars)
      ;; The state must be an abort state.
      ;; We need to try harder to get a character; even if
      ;; `zero-ok?` is true, we may need to explicitly ask
      ;; for more bytes to make any progress
      (let loop ([skip-k (+ skip-k (- v consumed-v))] [state state])
        (define v (peek-some-bytes! who orig-in bstr 0 1 skip-k
                                    #:zero-ok? zero-ok?))
        (cond
         [(eq? v 0) 0]
         [else
          ;; Try to convert with the additional byte; v can be `eof`,
          ;; in which case the abort mode should be 'error to trigger
          ;; decodings as errors
          (define-values (used-bytes got-chars new-state)
            (utf-8-decode! bstr 0 (if (eof-object? v) 0 v)
                           str start amt
                           #:error-char #\?
                           #:state state
                           #:abort-mode (if (eof-object? v)
                                            'error
                                            'state)))
          (cond
           [(zero? got-chars)
            ;; Try even harder; we shouldn't get here if v was `eof`
            (loop (+ skip-k v) new-state)]
           [else
            ;; At this point `used-bytes` can be negative, since
            ;; conversion may not have used all the bytes that
            ;; we peeked to try to complete a decoding
            (unless just-peek?
              (define actually-used-bytes (+ skip-k used-bytes))
              (unless (zero? actually-used-bytes)
                (define finish-bstr (if (actually-used-bytes . <= . (bytes-length bstr))
                                        bstr
                                        (make-bytes actually-used-bytes)))
                (do-read-bytes! who orig-in finish-bstr 0 actually-used-bytes)))
            got-chars])]))]
     [else
      ;; Conversion succeeded for at least 1 character. If there's
      ;; an issue for getting more, let another call to `read-some-chars`
      ;; deal with it.
      (define actually-used-bytes (- used-bytes (if (utf-8-state? state)
                                                    (utf-8-state-pending-amt state)
                                                    0)))
      (unless (or just-peek?
                  (= actually-used-bytes consumed-v))
        (do-read-bytes! who orig-in bstr 0 (- actually-used-bytes consumed-v)))
      got-chars])]))

;; ----------------------------------------

;; Read `(- end start)` chars, stopping early only if an EOF is found
(define (do-read-string! who in str start end
                         #:just-peek? [just-peek? #f]
                         #:skip [skip-k 0])
  (define amt (- end start))
  (define v (read-some-chars! who in str start end
                              #:just-peek? just-peek?
                              #:skip skip-k))
  (cond
   [(not (exact-integer? v)) v]
   [(= v amt) v]
   [else
    (let loop ([got v])
      (define v (read-some-chars! who in str got amt
                                  #:keep-eof? #t
                                  #:just-peek? just-peek?
                                  #:skip (if just-peek?
                                             (+ skip-k got)
                                             0)))
      (cond
       [(eof-object? v)
        got]
       [else
        (define new-got (+ got v))
        (cond
         [(= new-got amt) amt]
         [else (loop new-got)])]))]))

;; ----------------------------------------

;; A shortcut to implement `read-char` in terms of a port-specific
;; `read-byte`:
(define (read-char-via-read-byte who in read-byte)
  (define b (read-byte))
  (cond
   [(eof-object? b) b]
   [else
    (input-port-count-byte! in b)
    (cond
     [(b . < . 128) (integer->char b)]
     [else
      ;; UTF-8 decoding... May need to peek bytes to discover
      ;; whether the decoding will work (in which case this wasn't
      ;; much of a shortcut)
      (define bstr (bytes b))
      (define str (make-string 1))
      (define-values (used-bytes got-chars state)
        (utf-8-decode! bstr 0 1
                       #f 0 #f
                       #:abort-mode 'state))
      (cond
       [(eq? state 'error)
        ;; This happens if the byte is a UTF-8 continuation byte
        #\?]
       [else
        ;; Need to peek ahead
        (let loop ([skip-k 0] [state state])
          (define v (peek-some-bytes! who in bstr 0 1 skip-k #:copy-bstr? #f))
          (cond
           [(eof-object? v)
            ;; Already-consumed byte is an error byte
            #\?]
           [else
            (define-values (used-bytes got-chars new-state)
              (utf-8-decode! bstr 0 1
                             str 0 1
                             #:state state
                             #:error-char #\?
                             #:abort-mode 'state))
            (cond
             [(= got-chars 1)
              (define actually-used-bytes (+ skip-k used-bytes))
              (unless (zero? actually-used-bytes)
                (define finish-bstr (if (actually-used-bytes . <= . (bytes-length bstr))
                                        bstr
                                        (make-bytes actually-used-bytes)))
                (do-read-bytes! who in finish-bstr 0 actually-used-bytes))
              (string-ref str 0)]
             [else
              (loop (add1 skip-k) new-state)])]))])])]))

;; ----------------------------------------

(define (read-char [in (current-input-port)])
  (check 'read-char input-port? in)
  (define read-byte (input-port-read-byte in))
  (cond
   [(not read-byte)
    (define str (make-string 1))
    (define v (read-some-chars! 'read-char in str 0 1))
    (if (eq? v 1)
        (string-ref str 1)
        v)]
   [else
    ;; Byte-level shortcut is available, so try it as a char shortcut
    (read-char-via-read-byte 'read-char in read-byte)]))

(define (read-string amt [in (current-input-port)])
  (check 'read-string exact-nonnegative-integer? amt)
  (check 'read-string input-port? in)
  (define bstr (make-string amt))
  (define v (do-read-string! 'read-string in bstr 0 amt))
  (if (exact-integer? v)
      (if (= v amt)
          bstr
          (substring bstr 0 v))
      v))

(define (read-string! str [in (current-input-port)] [start-pos 0] [end-pos (and (string? str)
                                                                                (string-length str))])
  (check 'read-string! string? str)
  (check 'read-string! input-port? in)
  (check 'read-string! exact-nonnegative-integer? start-pos)
  (check 'read-string! exact-nonnegative-integer? end-pos)
  (check-range 'read-string! start-pos end-pos (string-length str) str)
  (do-read-string! 'read-string! in str start-pos end-pos))

;; ----------------------------------------

(define (do-peek-string! who in str start end skip)
  (do-read-string! who in str start end #:skip skip #:just-peek? #t))

(define (peek-char [in (current-input-port)] [skip-k 0])
  (check 'peek-char input-port? in)
  (check 'peek-char exact-nonnegative-integer? skip-k)
  (define peek-byte (and (zero? skip-k)
                         (input-port-read-byte in)))
  (define b (and peek-byte (peek-byte)))
  (cond
   [(and b
         (or (eof-object? b)
             (b . < . 128)))
    ;; Shortcut worked
    (integer->char b)]
   [else
    ;; General mode
    (define bstr (make-string 1))
    (define v (do-peek-string! 'peek-char in bstr 0 1 skip-k))
    (if (eq? v 1)
        (string-ref bstr 1)
        v)]))

(define (peek-string amt skip-k [in (current-input-port)])
  (check 'peek-string exact-nonnegative-integer? amt)
  (check 'peek-string exact-nonnegative-integer? skip-k)
  (check 'peek-string input-port? in)
  (define bstr (make-string amt))
  (define v (do-peek-string! 'peek-string bstr in 0 amt skip-k))
  (if (exact-integer? v)
      (if (= v amt)
          bstr
          (substring bstr 0 v))
      v))

(define (peek-string! str skip-k [in (current-input-port)] [start-pos 0] [end-pos (and (string? str)
                                                                                       (string-length str))])
  (check 'peek-string! string? str)
  (check 'peek-string! exact-nonnegative-integer? skip-k)
  (check 'peek-string! input-port? in)
  (check 'peek-string! exact-nonnegative-integer? start-pos)
  (check 'peek-string! exact-nonnegative-integer? end-pos)
  (check-range 'peek-string! start-pos end-pos (string-length str) str)
  (do-peek-string! 'peek-string! str in start-pos end-pos skip-k))

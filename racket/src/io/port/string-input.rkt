#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "parameter.rkt"
         "read-and-peek.rkt"
         "input-port.rkt"
         (submod "bytes-input.rkt" internal)
         "../string/utf-8-decode.rkt"
         "count.rkt")

(provide read-char
         read-string
         read-string!
         
         peek-char
         peek-string
         peek-string!

         do-read-char
         do-peek-char)

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
                          #:skip [skip-k 0] ; must be 0 if `(not just-peek?)`
                          #:special-ok? [special-ok? #f])
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
                          #:keep-eof? keep-eof?
                          #:special-ok? special-ok?)
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
                      str start (+ start amt)
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
                                      #:zero-ok? zero-ok?
                                      #:special-ok? special-ok?))
          (cond
            [(eq? v 0) 0]
            [else
             ;; Try to convert with the additional byte; v can be
             ;; `eof` or a special-value procedure, in which case the
             ;; abort mode should be 'error to trigger decodings as
             ;; errors
             (define-values (used-bytes got-chars new-state)
               (utf-8-decode! bstr 0 (if (integer? v) v 0)
                              str start amt
                              #:error-char #\?
                              #:state state
                              #:abort-mode (if (integer? v)
                                               'state
                                               'error)))
             (cond
               [(zero? got-chars)
                ;; Try even harder; we shouldn't get here if v was `eof`
                ;; or a special-value procedure
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
                         #:skip [skip-k 0]
                         #:special-ok? [special-ok? #f])
  (define amt (- end start))
  (define v (read-some-chars! who in str start end
                              #:just-peek? just-peek?
                              #:skip skip-k
                              #:special-ok? special-ok?))
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
(define (read-char-via-read-byte who in read-byte #:special-ok? [special-ok? #t])
  (define b
    (let loop ()
      (start-atomic)
      (define b (read-byte))
      (cond
        [(evt? b)
         (end-atomic)
         (sync b)
         (loop)]
        [else
         (unless (eof-object? b)
           (port-count-byte! in b))
         (end-atomic)
         b])))
  (cond
    [(eof-object? b) b]
    [else
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
             (define v (peek-some-bytes! who in bstr 0 1 skip-k #:copy-bstr? #f #:special-ok? special-ok?))
             (cond
               [(or (eof-object? v)
                    (procedure? v))
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

;; If `special-ok?`, can return a special-value procedure
(define (do-read-char who in #:special-ok? [special-ok? #f])
  (check who input-port? in)
  (let ([in (->core-input-port in)])
    (define read-byte (core-input-port-read-byte in))
    (cond
      [(not read-byte)
       (define str (make-string 1))
       (define v (read-some-chars! who in str 0 1 #:special-ok? special-ok?))
       (if (eq? v 1)
           (string-ref str 0)
           v)]
      [else
       ;; Byte-level shortcut is available, so try it as a char shortcut
       (read-char-via-read-byte who in read-byte #:special-ok? special-ok?)])))

(define/who (read-char [in (current-input-port)])
  (check who input-port? in)
  (do-read-char who in))
  
(define/who (read-string amt [in (current-input-port)])
  (check who exact-nonnegative-integer? amt)
  (check who input-port? in)
  (let ([in (->core-input-port in)])
    (define bstr (make-string amt))
    (define v (do-read-string! 'read-string in bstr 0 amt))
    (if (exact-integer? v)
        (if (= v amt)
            bstr
            (substring bstr 0 v))
        v)))

(define/who (read-string! str [in (current-input-port)] [start-pos 0] [end-pos (and (string? str)
                                                                                    (string-length str))])
  (check who string? str)
  (check who input-port? in)
  (check who exact-nonnegative-integer? start-pos)
  (check who exact-nonnegative-integer? end-pos)
  (check-range who start-pos end-pos (string-length str) str)
  (let ([in (->core-input-port in)])
    (do-read-string! who in str start-pos end-pos)))

;; ----------------------------------------

(define (do-peek-string! who in str start end skip #:special-ok? [special-ok? #f])
  (do-read-string! who in str start end #:skip skip #:just-peek? #t #:special-ok? special-ok?))

(define (do-peek-char who in skip-k #:special-ok? [special-ok? #f])
  (let ([in (->core-input-port in)])
    (define peek-byte (and (zero? skip-k)
                           (core-input-port-peek-byte in)))
    (define b (and peek-byte (peek-byte)))
    (cond
      [(and b
            (or (eof-object? b)
                (b . < . 128)))
       ;; Shortcut worked
       (if (eof-object? b) b (integer->char b))]
      [else
       ;; General mode
       (define bstr (make-string 1))
       (define v (do-peek-string! who in bstr 0 1 skip-k #:special-ok? special-ok?))
       (if (eq? v 1)
           (string-ref bstr 0)
           v)])))

(define/who (peek-char [in (current-input-port)] [skip-k 0])
  (check who input-port? in)
  (check who exact-nonnegative-integer? skip-k)
  (do-peek-char who in skip-k #:special-ok? #f))
  
(define/who (peek-string amt skip-k [in (current-input-port)])
  (check who exact-nonnegative-integer? amt)
  (check who exact-nonnegative-integer? skip-k)
  (check who input-port? in)
  (let ([in (->core-input-port in)])
    (define bstr (make-string amt))
    (define v (do-peek-string! who bstr in 0 amt skip-k))
    (if (exact-integer? v)
        (if (= v amt)
            bstr
            (substring bstr 0 v))
        v)))

(define/who (peek-string! str skip-k [in (current-input-port)] [start-pos 0] [end-pos (and (string? str)
                                                                                           (string-length str))])
  (check who string? str)
  (check who exact-nonnegative-integer? skip-k)
  (check who input-port? in)
  (check who exact-nonnegative-integer? start-pos)
  (check who exact-nonnegative-integer? end-pos)
  (check-range who start-pos end-pos (string-length str) str)
  (let ([in (->core-input-port in)])
    (do-peek-string! who str in start-pos end-pos skip-k)))

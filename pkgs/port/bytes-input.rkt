#lang racket/base
(require "read-and-peek.rkt"
         "input-port.rkt"
         "internal-pipe.rkt"
         "check.rkt"
         "count.rkt")

(provide read-byte
         read-bytes
         read-bytes!
         read-bytes-avail!
         read-bytes-avail!*
         
         peek-byte
         peek-bytes
         peek-bytes!
         peek-bytes-avail!
         peek-bytes-avail!*)

(module+ internal
  (provide do-read-bytes!))

;; ----------------------------------------

;; Read `(- end start)` bytes, stopping early only if an EOF is found
(define (do-read-bytes! who in bstr start end)
  (define amt (- end start))
  (define v (read-some-bytes! who in bstr start end))
  (cond
   [(not (exact-integer? v)) v]
   [(= v amt) v]
   [else
    (let loop ([got v])
      (define v (read-some-bytes! who in bstr got amt #:keep-eof? #t))
      (cond
       [(eof-object? v)
        got]
       [else
        (define new-got (+ got v))
        (cond
         [(= new-got amt) amt]
         [else (loop new-got)])]))]))

;; ----------------------------------------

(define (read-byte [in (current-input-port)])
  (check 'read-byte input-port? in)
  (define read-byte (input-port-read-byte in))
  (cond
   [read-byte
    ;; Shortcut is available
    (define b (read-byte))
    (unless (eof-object? b) (input-port-count-byte! in b))
    b]
   [else
    ;; Use the general path
    (define bstr (make-bytes 1))
    (define v (read-some-bytes! 'read-byte in bstr 0 1 #:copy-bstr? #f))
    (if (eq? v 1)
        (bytes-ref bstr 1)
        v)]))

(define (read-bytes amt [in (current-input-port)])
  (check 'read-bytes exact-nonnegative-integer? amt)
  (check 'read-bytes input-port? in)
  (define bstr (make-bytes amt))
  (define v (do-read-bytes! 'read-bytes in bstr 0 amt))
  (if (exact-integer? v)
      (if (= v amt)
          bstr
          (subbytes bstr 0 v))
      v))

(define (read-bytes! bstr [in (current-input-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                (bytes-length bstr))])
  (check 'read-bytes! bytes? bstr)
  (check 'read-bytes! input-port? in)
  (check 'read-bytes! exact-nonnegative-integer? start-pos)
  (check 'read-bytes! exact-nonnegative-integer? end-pos)
  (check-range 'read-bytes! start-pos end-pos (bytes-length bstr) bstr)
  (do-read-bytes! 'read-bytes! in bstr start-pos end-pos))

(define (read-bytes-avail! bstr [in (current-input-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                      (bytes-length bstr))])
  (check 'read-bytes-avail! bytes? bstr)
  (check 'read-bytes-avail! input-port? in)
  (check 'read-bytes-avail! exact-nonnegative-integer? start-pos)
  (check 'read-bytes-avail! exact-nonnegative-integer? end-pos)
  (check-range 'read-bytes-avail! start-pos end-pos (bytes-length bstr) bstr)
  (read-some-bytes! 'read-bytes-avail! in bstr start-pos end-pos))

(define (read-bytes-avail!* bstr [in (current-input-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                       (bytes-length bstr))])
  (check 'read-bytes-avail!* bytes? bstr)
  (check 'read-bytes-avail!* input-port? in)
  (check 'read-bytes-avail!* exact-nonnegative-integer? start-pos)
  (check 'read-bytes-avail!* exact-nonnegative-integer? end-pos)
  (check-range 'read-bytes-avail!* start-pos end-pos (bytes-length bstr) bstr)
  (read-some-bytes! 'read-bytes-avail!* in bstr start-pos end-pos #:zero-ok? #t))

;; ----------------------------------------

;; Peek `(- end start)` bytes, stopping early only if an EOF is found
(define (do-peek-bytes! who in bstr start end skip)
  (define amt (- end start))
  (define v (peek-some-bytes! who in bstr start end skip))
  (if (exact-integer? v)
      (cond
       [(= v amt) v]
       [else
        (let loop ([got v])
          (define v (peek-some-bytes! who in bstr got amt (+ got skip) #:copy-bstr? #f))
          (cond
           [(eof-object? v)
            got]
           [else
            (define new-got (+ got v))
            (cond
             [(= new-got amt) amt]
             [else (loop new-got)])]))])
      v))

(define (peek-byte [in (current-input-port)] [skip-k 0])
  (check 'peek-byte input-port? in)
  (check 'peek-byte exact-nonnegative-integer? skip-k)
  (define peek-byte (and (zero? skip-k)
                         (input-port-read-byte in)))
  (cond
   [peek-byte
    ;; Shortcut is available
    (peek-byte)]
   [else
    ;; Use the general path
    (define bstr (make-bytes 1))
    (define v (peek-some-bytes! 'peek-byte in bstr 0 1 skip-k #:copy-bstr? #f))
    (if (eq? v 1)
        (bytes-ref bstr 1)
        v)]))

(define (peek-bytes amt skip-k [in (current-input-port)])
  (check 'peek-bytes exact-nonnegative-integer? amt)
  (check 'peek-bytes exact-nonnegative-integer? skip-k)
  (check 'peek-bytes input-port? in)
  (define bstr (make-bytes amt))
  (define v (do-peek-bytes! 'read-bytes in bstr 0 amt skip-k))
  (if (exact-integer? v)
      (if (= v amt)
          bstr
          (subbytes bstr 0 v))
      v))

(define (peek-bytes! bstr skip-k [in (current-input-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                       (bytes-length bstr))])
  (check 'peek-bytes! bytes? bstr)
  (check 'peek-bytes! exact-nonnegative-integer? skip-k)
  (check 'peek-bytes! input-port? in)
  (check 'peek-bytes! exact-nonnegative-integer? start-pos)
  (check 'peek-bytes! exact-nonnegative-integer? end-pos)
  (check-range 'peek-bytes! start-pos end-pos (bytes-length bstr) bstr)
  (do-peek-bytes! 'peek-bytes! in bstr start-pos end-pos skip-k))

(define (peek-bytes-avail! bstr skip-k [in (current-input-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                             (bytes-length bstr))])
  (check 'peek-bytes-avail! bytes? bstr)
  (check 'peek-bytes-avail! exact-nonnegative-integer? skip-k)
  (check 'peek-bytes-avail! input-port? in)
  (check 'peek-bytes-avail! exact-nonnegative-integer? start-pos)
  (check 'peek-bytes-avail! exact-nonnegative-integer? end-pos)
  (check-range 'peek-bytes-avail! start-pos end-pos (bytes-length bstr) bstr)
  (peek-some-bytes! 'peek-bytes-avail! bstr in start-pos end-pos skip-k))

(define (peek-bytes-avail!* bstr skip-k [in (current-input-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                              (bytes-length bstr))])
  (check 'peek-bytes-avail!* bytes? bstr)
  (check 'peek-bytes-avail! exact-nonnegative-integer? skip-k)
  (check 'peek-bytes-avail!* input-port? in)
  (check 'peek-bytes-avail!* exact-nonnegative-integer? start-pos)
  (check 'peek-bytes-avail!* exact-nonnegative-integer? end-pos)
  (check-range 'peek-bytes-avail!* start-pos end-pos (bytes-length bstr) bstr)
  (peek-some-bytes! 'peek-bytes-avail!* bstr in start-pos end-pos skip-k #:zero-ok? #t))

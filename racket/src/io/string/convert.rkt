#lang racket/base
(require "utf-8-decode.rkt"
         "utf-8-encode.rkt"
         "../common/check.rkt")

(provide bytes->string/latin-1
         bytes->string/utf-8
         bytes->string/locale
         bytes-utf-8-length
         
         string->bytes/latin-1
         string->bytes/utf-8
         string->bytes/locale
         string-utf-8-length)

;; ----------------------------------------

(define (bytes->string/latin-1 bstr [err-char #f] [start 0] [end (and (bytes? bstr)
                                                                      (bytes-length bstr))])
  (check 'bytes->string/latin-1 bytes? bstr)
  (check 'bytes->string/latin-1 (lambda (v) (or (not v) (char? v)))
         #:contract "(or/c char? #f)" err-char)
  (check 'bytes->string/latin-1 exact-nonnegative-integer? start)
  (check 'bytes->string/latin-1 exact-nonnegative-integer? end)
  (check-range 'bytes->string/latin-1 start end (bytes-length bstr) bstr)
  (define len (- end start))
  (define s (make-string len))
  (let loop ([i len])
    (unless (zero? i)
      (let ([i (sub1 i)])
        (string-set! s i (integer->char (bytes-ref bstr (+ i start))))
        (loop i))))
  s)

(define (do-bytes->string/utf-8 who bstr err-char start end #:just-length? [just-length? #f])
  (check who bytes? bstr)
  (check who (lambda (v) (or (not v) (char? v)))
         #:contract "(or/c char? #f)" err-char)
  (check who exact-nonnegative-integer? start)
  (check who exact-nonnegative-integer? end)
  (check-range who start end (bytes-length bstr) bstr)
  ;; Measure result string:
  (define-values (used-bytes got-chars state)
    (utf-8-decode! bstr start end
                   #f 0 #f 
                   #:error-char err-char
                   #:abort-mode 'error))
  (cond
   [just-length? got-chars]
   [else
    ;; Create result string:
    (define str (make-string got-chars))
    (utf-8-decode! bstr start end
                   str 0 #f
                   #:error-char err-char
                   #:abort-mode 'error)
    str]))

(define (bytes->string/utf-8 bstr [err-char #f] [start 0] [end (and (bytes? bstr)
                                                                    (bytes-length bstr))])
  (do-bytes->string/utf-8 'bytes->string/utf-8 bstr err-char start end))

(define (bytes-utf-8-length bstr [err-char #f] [start 0] [end (and (bytes? bstr)
                                                                   (bytes-length bstr))])
  (do-bytes->string/utf-8 'bytes-utf-8-length bstr err-char start end #:just-length? #t))

;; For now, always use UTF-8 as locale encoding
(define (bytes->string/locale bstr [err-char #f] [start 0] [end (and (bytes? bstr)
                                                                    (bytes-length bstr))])
  (do-bytes->string/utf-8 'bytes->string/locale bstr err-char start end))

;; ----------------------------------------

(define (string->bytes/latin-1 str [err-byte #f] [start 0] [end (and (string? str)
                                                                     (string-length str))])
  (check 'string->bytes/latin-1 string? str)
  (check 'string->bytes/latin-1 (lambda (v) (or (not v) (byte? v)))
         #:contract "(or/c byte? #f)" err-byte)
  (check 'string->bytes/latin-1 exact-nonnegative-integer? start)
  (check 'string->bytes/latin-1 exact-nonnegative-integer? end)
  (check-range 'string->bytes/latin-1 start end (string-length str) str)
  (define len (- end start))
  (define bstr (make-bytes len))
  (let loop ([i len])
    (unless (zero? i)
      (let ([i (sub1 i)])
        (define b (char->integer (string-ref str (+ i start))))
        (cond
         [(byte? b) (bytes-set! bstr i b)]
         [err-byte (bytes-set! bstr i err-byte)]
         [else (raise-arguments-error 'string->bytes/latin-1
                                      "string cannot be encoded in Latin-1"
                                      "string" str)])
        (loop i))))
  bstr)

(define (do-string->bytes/utf-8 who str err-byte start end #:just-length? [just-length? #f])
  (check who string? str)
  (check who (lambda (v) (or (not v) (byte? v))) #:contract "(or/c byte? #f)" err-byte)
  (check who exact-nonnegative-integer? start)
  (check who exact-nonnegative-integer? end)
  (check-range who start end (string-length str) str)
  ;; Measure result byte string:
  (define-values (used-chars got-bytes status)
    (utf-8-encode! str start end
                   #f 0 #f))
  (cond
   [just-length? got-bytes]
   [else
    ;; Create result byte string:
    (define bstr (make-bytes got-bytes))
    (utf-8-encode! str start end
                   bstr 0 #f)
    bstr]))

(define (string->bytes/utf-8 str [err-byte #f] [start 0] [end (and (string? str)
                                                                   (string-length str))])
  (do-string->bytes/utf-8 'string->bytes/utf-8 str err-byte start end))

(define (string-utf-8-length str [start 0] [end (and (string? str)
                                                     (string-length str))])
  (do-string->bytes/utf-8 'string-utf-8-length str #f start end #:just-length? #t))

;; For now, always use UTF-8 as locale encoding
(define (string->bytes/locale str [err-byte #f] [start 0] [end (and (string? str)
                                                                    (string-length str))])
  (do-string->bytes/utf-8 'string->bytes/locale str err-byte start end))

#lang racket/base
(require "../common/check.rkt"
         "output-port.rkt"
         "parameter.rkt"
         "write.rkt")

(provide write-bytes
         write-bytes-avail
         write-bytes-avail*
         write-bytes-avail/enable-break
         write-bytes-avail-evt
         port-writes-atomic?)

(module+ internal
  (provide do-write-bytes))

(define (write-byte b [out (current-output-port)])
  (check 'write-bytes byte? b)
  (check 'write-bytes output-port? out)
  (let ([out (->core-output-port out)])
    (write-some-bytes 'write-byte out (bytes b) 0 1 #:buffer-ok? #t)))

(define (do-write-bytes who out bstr start end)
  (let loop ([i start])
    (cond
      [(= i end) (- i start)]
      [else
       (define n (write-some-bytes who out bstr i end #:buffer-ok? #t))
       (loop (+ n i))])))

(define (write-bytes bstr [out (current-output-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                  (bytes-length bstr))])
  (check 'write-bytes bytes? bstr)
  (check 'write-bytes output-port? out)
  (check 'write-bytes exact-nonnegative-integer? start-pos)
  (check 'write-bytes exact-nonnegative-integer? end-pos)
  (check-range 'write-bytes start-pos end-pos (bytes-length bstr) bstr)
  (let ([out (->core-output-port out)])
    (do-write-bytes 'write-bytes out bstr start-pos end-pos)))

(define (write-bytes-avail bstr [out (current-output-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                        (bytes-length bstr))])
  (check 'write-bytes-avail bytes? bstr)
  (check 'write-bytes-avail output-port? out)
  (check 'write-bytes-avail exact-nonnegative-integer? start-pos)
  (check 'write-bytes-avail exact-nonnegative-integer? end-pos)
  (check-range 'write-bytes-avail start-pos end-pos (bytes-length bstr) bstr)
  (let ([out (->core-output-port out)])
    (write-some-bytes 'write-bytes-avail out bstr start-pos end-pos)))

(define (write-bytes-avail* bstr [out (current-output-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                         (bytes-length bstr))])
  (check 'write-bytes-avail* bytes? bstr)
  (check 'write-bytes-avail* output-port? out)
  (check 'write-bytes-avail* exact-nonnegative-integer? start-pos)
  (check 'write-bytes-avail* exact-nonnegative-integer? end-pos)
  (check-range 'write-bytes-avail* start-pos end-pos (bytes-length bstr) bstr)
  (let ([out (->core-output-port out)])
    (write-some-bytes 'write-bytes-avail* out bstr start-pos end-pos #:zero-ok? #t)))

(define (write-bytes-avail/enable-break bstr [out (current-output-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                                     (bytes-length bstr))])
  (check 'write-bytes-avail/enable-break bytes? bstr)
  (check 'write-bytes-avail/enable-break output-port? out)
  (check 'write-bytes-avail/enable-break exact-nonnegative-integer? start-pos)
  (check 'write-bytes-avail/enable-break exact-nonnegative-integer? end-pos)
  (check-range 'write-bytes-avail/enable-break start-pos end-pos (bytes-length bstr) bstr)
  (let ([out (->core-output-port out)])
    (write-some-bytes 'write-bytes-avail/enable-break out bstr start-pos end-pos #:enable-break? #t)))

(define (write-bytes-avail-evt bstr [out (current-output-port)] [start-pos 0] [end-pos (and (bytes? bstr)
                                                                                            (bytes-length bstr))])
  (check 'write-bytes-avail-evt bytes? bstr)
  (check 'write-bytes-avail-evt output-port? out)
  (check 'write-bytes-avail-evt exact-nonnegative-integer? start-pos)
  (check 'write-bytes-avail-evt exact-nonnegative-integer? end-pos)
  (check-range 'write-bytes-avail-evt start-pos end-pos (bytes-length bstr) bstr)
  (let ([out (->core-output-port out)])
    (define get-write-evt (core-output-port-get-write-evt out))
    (unless get-write-evt
      (raise-arguments-error 'write-bytes-avail-evt
                             "port does not support output events"
                             "port" out))
    (get-write-evt bstr start-pos end-pos)))

(define (port-writes-atomic? out)
  (check 'port-writes-atomic? output-port? out)
  (let ([out (->core-output-port out)])
    (and (core-output-port-get-write-evt out) #t)))

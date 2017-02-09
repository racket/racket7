#lang racket/base
(require "input-port.rkt"
         "output-port.rkt"
         "utf-8.rkt"
         "check.rkt")

(provide port-count-lines!
         port-next-location
         
         input-port-count!
         input-port-count-byte!)

(define (port-count-lines! p)
  (cond
   [(input-port? p)
    (unless (input-port-line p)
      (set-input-port-line! p 1)
      (set-input-port-column! p 0)
      (set-input-port-position! p 1)
      (define count-lines! (input-port-count-lines! p))
      (when count-lines! (count-lines!)))]
   [(output-port?)
    (unless (output-port-line p)
      (set-output-port-line! p 1)
      (set-output-port-column! p 0)
      (set-output-port-position! p 1)
      (define count-lines! (output-port-count-lines! p))
      (when count-lines! (count-lines!)))]
   [else
    (check 'port-count-lines! (lambda (x) #f)
           #:contract "(or/c input-port? output-port?)"
           p)]))

(define (port-next-location p)
  (cond
   [(input-port? p)
    (values (input-port-line p)
            (input-port-column p)
            (input-port-position p))]
   [(output-port? p)
    (values (output-port-line p)
            (output-port-column p)
            (output-port-position p))]
   [else
    (check 'port-next-location (lambda (x) #f)
           #:contract "(or/c input-port? output-port?)"
           p)]))

;; When line counting is enabled, increment line, column, etc. counts,
;; which involves UTF-8 decoding
(define (input-port-count! in amt bstr start)
  (set-input-port-offset! in (+ amt (input-port-offset in)))
  (when (input-port-line in)
    (define end (+ start amt))
    (let loop ([i start]
               [span 0] ; number of previous bytes still to send to UTF-8 decoding
               [line (input-port-line in)]
               [column (input-port-column in)]
               [position (input-port-position in)]
               [state (input-port-state in)]
               [cr-state (input-port-cr-state in)]) ; #t => previous char was #\return
      (cond
       [(= i end)
        (cond
         [(zero? span)
          (set-input-port-line! in line)
          (set-input-port-column! in column)
          (set-input-port-position! in position)
          (set-input-port-state! in state)
          (set-input-port-cr-state! in cr-state)]
         [else
          ;; span doesn't include CR, LF, or tab
          (define-values (used-bytes got-chars new-state)
            (utf-8-decode! bstr (- end span) end
                           #f 0 #f
                           #:error-char #\?
                           #:abort-mode 'state
                           #:state state))
          (loop end line (+ column got-chars) (+ position got-chars) new-state #f)])]
       [else
        (define b (bytes-ref bstr i))
        (define (end-utf-8) ; => next byte is ASCII, so we can terminate a UTF-8 sequence
          (define-values (used-bytes got-chars new-state)
            (utf-8-decode! bstr (- i span) i
                           #f 0 #f
                           #:error-char #\?
                           #:state state))
          (loop end line (+ column got-chars) (+ position got-chars) new-state #f))
        (cond
         [(eq? b (char->integer #\newline))
          (cond
           [(or state (not (zero? span))) (end-utf-8)]
           [cr-state
            (loop (add1 i) 0 line column (add1 position) #f #f)]
           [else
            (loop (add1 i) 0 (add1 line) 0 (add1 position) #f #f)])]
         [(eq? b (char->integer #\return))
          (if (and (zero? span)(not state))
              (loop (add1 i) 0 (add1 line) 0 (add1 position) #f #t)
              (end-utf-8))]
         [(eq? b (char->integer #\tab))
          (if (and (zero? span) (not state))
              (loop (add1 i) 0 line (+ column (bitwise-and (+ column 7) -8)) (add1 position) #f #f)
              (end-utf-8))]
         [(b . < . 128)
          (if (and (zero? span) (not state))
              (loop (add1 i) 0 line (add1 column) (add1 position) #f #f)
              (loop (add1 i) (add1 span) line column position state state #f))]
         [else
          (loop (add1 i) (add1 span) line column position state #f)])]))))
  
(define (input-port-count-byte! in b)
  (set-input-port-offset! in (add1 (input-port-offset in)))
  (when (input-port-line in)
    (cond
     [(or (input-port-state in)
          (input-port-cr-state in)
          (eq? b (char->integer #\return))
          (eq? b (char->integer #\newline))
          (eq? b (char->integer #\tab)))
      (input-port-count! in 1 (bytes b) 0)]
     [else
      (set-input-port-position! in (add1 (input-port-position in)))
      (set-input-port-column! in (add1 (input-port-column in)))])))

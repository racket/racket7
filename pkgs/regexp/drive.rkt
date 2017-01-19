#lang racket/base
(require "regexp.rkt"
         "lazy-bytes.rkt"
         "port.rkt")

;; Drives a regexp matcher on a byte string, character string, or port

(provide drive-regexp-match
         
         fast-drive-regexp-match?/bytes
         fast-drive-regexp-match?/string
         fast-drive-regexp-match-positions/bytes
         fast-drive-regexp-match-positions/string
         fast-drive-regexp-match/bytes
         fast-drive-regexp-match/string

         FAST-STRING-LEN)

;; ----------------------------------------
;; Start with some (repetative) functions for the most common cases to
;; keep the overhead low for reaching these cases.

(define FAST-STRING-LEN 64)

(define (fast-drive-regexp-match?/bytes rx in start-pos end-pos)
  (define-values (ms-pos me-pos)
    (search-match rx in start-pos start-pos (or end-pos (bytes-length in)) #f))
  (and ms-pos #t))

(define (fast-drive-regexp-match?/string rx in-str start-pos end-pos)
  (define in (string->bytes/utf-8 in-str 0 start-pos (or end-pos (string-length in-str))))
  (define-values (ms-pos me-pos)
    (search-match rx in start-pos start-pos (or end-pos (string-length in)) #f))
  (and ms-pos #t))

(define (fast-drive-regexp-match-positions/bytes rx in start-pos end-pos)
  (define state (let ([n (rx:regexp-num-groups rx)])
                  (and (positive? n)
                       (make-vector n #f))))
  (define-values (ms-pos me-pos)
    (search-match rx in start-pos start-pos (or end-pos (bytes-length in)) state))
  (and ms-pos
       (if state
           (cons (cons ms-pos me-pos) (vector->list state))
           (list (cons ms-pos me-pos)))))

(define (fast-drive-regexp-match-positions/string rx in-str start-pos end-pos)
  (define in (string->bytes/utf-8 in-str 0 start-pos (or end-pos (string-length in-str))))
  (define state (let ([n (rx:regexp-num-groups rx)])
                  (and (positive? n)
                       (make-vector n #f))))
  (define-values (ms-pos me-pos)
    (search-match rx in 0 0 (bytes-length in) state))
  (define (string-pos pos)
    (+ start-pos (bytes-utf-8-length in #\? 0 pos)))
  (and ms-pos
       (cons (cons (string-pos ms-pos) (string-pos me-pos))
             (if state
                 (for/list ([p (in-vector state)])
                   (and p
                        (cons (string-pos (car p))
                              (string-pos (cdr p)))))
                 null))))

(define (fast-drive-regexp-match/bytes rx in start-pos end-pos)
  (define state (let ([n (rx:regexp-num-groups rx)])
                  (and (positive? n)
                       (make-vector n #f))))
  (define-values (ms-pos me-pos)
    (search-match rx in start-pos start-pos (or end-pos (bytes-length in)) state))
  (and ms-pos
       (cons (subbytes in ms-pos me-pos)
             (if state
                 (for/list ([p (in-vector state)])
                   (and p
                        (subbytes in (car p) (cdr p))))
                 null))))

(define (fast-drive-regexp-match/string rx in-str start-pos end-pos)
  (define in (string->bytes/utf-8 in-str 0 start-pos (or end-pos (string-length in-str))))
  (define state (let ([n (rx:regexp-num-groups rx)])
                  (and (positive? n)
                       (make-vector n #f))))
  (define-values (ms-pos me-pos)
    (search-match rx in 0 0 (bytes-length in) state))
  (and ms-pos
       (cons (bytes->string/utf-8 in #\? ms-pos me-pos)
             (if state
                 (for/list ([p (in-vector state)])
                   (and p
                        (bytes->string/utf-8 in #\? (car p) (cdr p))))
                 null))))

;; ----------------------------------------
;; The general case

(define (drive-regexp-match who orig-rx in orig-start-pos orig-end-pos out prefix
                            #:mode mode
                            #:in-port-ok? [in-port-ok? #t]
                            #:peek? [peek? #f] #:immediate-only? [immediate-only? #f]
                            #:progress-evt [progress-evt #f]
                            #:end-bytes? [end-bytes? #f]
                            #:end-bytes-count [end-bytes-count #f])
  
  (define rx (cond
              [(rx:regexp? orig-rx) orig-rx]
              [(string? orig-rx) (make-regexp who orig-rx #f #f #f)]
              [(bytes? orig-rx) (make-regexp who orig-rx #f #t #f)]
              [else (raise-argument-error who "(or/c regexp? byte-regexp? string? bytes?)" orig-rx)]))
  (unless (or (and (bytes? in) (not peek?))
              (and (string? in) (not peek?))
              (and in-port-ok? (input-port? in)))
    (raise-argument-error who
                          (cond
                           [peek? "input-port?"]
                           [in-port-ok? "(or/c bytes? string? input-port?)"]
                           [else "(or/c bytes? string?)"])
                          in))
  
  (define start-offset (cond
                        [orig-start-pos
                         (unless (exact-nonnegative-integer? orig-start-pos)
                           (raise-argument-error who "exact-nonnegative-integer?" orig-start-pos))
                         (check-range who "starting index" in orig-start-pos 0)
                         orig-start-pos]
                        [else 0]))
  (define end-offset (cond
                      [orig-end-pos
                       (unless (exact-nonnegative-integer? orig-end-pos)
                         (raise-argument-error who "(or/c #f exact-nonnegative-integer?)" orig-end-pos))
                       (check-range who "ending index" in orig-end-pos start-offset)
                       orig-end-pos]
                      [(bytes? in) (bytes-length in)]
                      [(string? in) (string-length in)]
                      [else 'eof]))
  
  (unless (or (not out) (output-port? out))
    (raise-argument-error who "(or/c #f output-port?)" out))
  
  (unless (bytes? prefix)
    (raise-argument-error who "bytes?" prefix))
  
  (when end-bytes?
    (unless (exact-nonnegative-integer? end-bytes-count)
      (raise-argument-error who "exact-nonnegative-integer?" end-bytes-count)))
  
  (define state (and (not (eq? mode '?))
                     (let ([n (rx:regexp-num-groups rx)])
                       (and (positive? n)
                            (make-vector n #f)))))
  
  (define (add-end-bytes bstr me-pos results)
    (if end-bytes-count
        (values results
                (and results
                     (subbytes bstr (max 0 (- me-pos end-bytes-count)) me-pos)))
        results))
  
  ;; Separate cases for bytes, strings, and port.
  ;; There's an annoying level of duplication here, but
  ;; there are lots of little differences in each case
  (cond
   
   [(and (bytes? in)
         (not out)
         (equal? #"" prefix))
    (define start-pos start-offset)
    (define end-pos end-offset)
    (define-values (ms-pos me-pos)
      (search-match rx in start-pos start-pos end-pos state))
    (when out
      (write-bytes in out 0 (or ms-pos end-pos)))
    (case (and ms-pos mode)
      [(#f) (add-end-bytes #f #f #f)]
      [(?) #t]
      [(positions)
       (add-end-bytes
        in me-pos
        (if state
            (cons (cons ms-pos me-pos) (vector->list state))
            (list (cons ms-pos me-pos))))]
      [(strings)
       (add-end-bytes
        in me-pos
        (cons (subbytes in ms-pos me-pos)
              (if state
                  (for/list ([p (in-vector state)])
                    (and p
                         (subbytes in (car p) (cdr p))))
                  null)))])]

   [(and (string? in)
         (not out)
         (equal? #"" prefix)
         ((- end-offset start-offset) . < . FAST-STRING-LEN))
    (define bstr-in (string->bytes/utf-8 in 0 start-offset end-offset))
    (define start-pos 0)
    (define end-pos (bytes-length bstr-in))
    (define-values (ms-pos me-pos)
      (search-match rx bstr-in 0 0 end-pos state))
    (define (string-pos pos)
      (+ start-offset (bytes-utf-8-length bstr-in #\? 0 pos)))
    (when out
      (write-string in out 0 start-offset)
      (write-bytes bstr-in out 0 (or ms-pos end-pos)))
    (case (and ms-pos mode)
      [(#f) (add-end-bytes #f #f #f)]
      [(?) #t]
      [(positions)
       (add-end-bytes
        bstr-in me-pos
        (cons (cons (string-pos ms-pos) (string-pos me-pos))
              (if state
                  (for/list ([p (in-vector state)])
                    (and p
                         (cons (string-pos (car p))
                               (string-pos (cdr p)))))
                  null)))]
      [(strings)
       (add-end-bytes
        bstr-in me-pos
        (cons (bytes->string/utf-8 bstr-in #\? ms-pos me-pos)
              (if state
                  (for/list ([p (in-vector state)])
                    (and p
                         (bytes->string/utf-8 bstr-in #\? (car p) (cdr p))))
                  null)))])]
   
   [else
    (define start-pos (bytes-length prefix))
    (define port-in
      (cond
       [(bytes? in) (open-input-bytes/no-copy in start-offset end-offset)]
       [(string? in) (open-input-string/lazy in start-offset end-offset)]
       [else in]))
    (when (and (not peek?) (positive? start-offset))
      ;; Consume bytes that we're skipping over:
      (cond
       [(bytes? in) (when out (write-bytes in out 0 start-offset))]
       [(string? in) (when out (write-string in out 0 start-offset))]
       [(input-port? in) (copy-port-bytes port-in out start-offset)]))
    ;; Create a lazy string from the port:
    (define lb-in (make-lazy-bytes port-in (if peek? start-offset 0) prefix
                                   peek? immediate-only? progress-evt
                                   out (rx:regexp-max-lookbehind rx)))
    (define end-pos (if (eq? 'eof end-offset)
                        eof
                        (+ start-pos end-offset)))
    (define prefix-len (bytes-length prefix))
    (define end-pos-in-lazy-bytes (if (eq? end-pos 'eof)
                                      'end-pos
                                      (+ (- end-pos start-offset) prefix-len)))
    ;; Run the matcher:
    (define-values (ms-pos me-pos)
      (search-match rx lb-in prefix-len prefix-len end-pos-in-lazy-bytes state))
    ;; Result positions correspond to the port after `start-offset`, 
    ;; but with the prefix bytes; note that the byte string may
    ;; be shifted by discarded bytes, if not in `peek?` mode
    (define (port-pos pos)
      (+ (- pos start-pos) start-offset))
    (define result
      (case (and ms-pos
                 (not (lazy-bytes-failed? lb-in))
                 mode)
        [(#f) (add-end-bytes #f #f #f)]
        [(?) #t]
        [(positions)
         (add-end-bytes
          (lazy-bytes-bstr lb-in) me-pos
          (cons (cons (port-pos ms-pos) (port-pos me-pos))
                (if state
                    (for/list ([p (in-vector state)])
                      (and p
                           (cons (port-pos (car p))
                                 (port-pos (cdr p)))))
                    null)))]
        [(strings)
         (define (bytes-pos pos)
           (- pos (lazy-bytes-discarded-count lb-in)))
         (define bstr (lazy-bytes-bstr lb-in))
         (add-end-bytes
          bstr me-pos
          (cons (subbytes bstr (bytes-pos ms-pos) (bytes-pos me-pos))
                (if state
                    (for/list ([p (in-vector state)])
                      (and p
                           (subbytes bstr (bytes-pos (car p)) (bytes-pos (cdr p)))))
                    null)))]))
    ;; Consume input
    (when (not peek?)
      (cond
       [ms-pos
        (when (or out (input-port? in))
          ;; Flush bytes before match:
          (lazy-bytes-advance! lb-in ms-pos #t)
          ;; Consume bytes that correspond to match:
          (copy-port-bytes port-in #f (- me-pos ms-pos)))]
       [(eq? end-pos 'eof)
        ;; copy all remaining bytes from input to output
        (copy-port-bytes port-in out #f)]
       [else
        (when (or out (input-port? in))
          (lazy-bytes-advance! lb-in end-pos-in-lazy-bytes #t))]))
    ;; Result is ready
    result]))

;; ----------------------------------------
;; The driver iterates through an input (unless the pattern is anchored)
;; to find a match

(define (search-match rx in pos start-pos end-pos state)
  (define matcher (rx:regexp-matcher rx))
  (define anchored? (rx:regexp-anchored? rx))
  (define must-string (rx:regexp-must-string rx))
  (cond
   [(not (check-must-string must-string in pos end-pos))
    (values #f #f)]
   [else
    (let loop ([pos pos])
      (cond
       [(and anchored? (not (= pos start-pos)))
        (values #f #f)]
       [else
        (define pos2 (matcher in pos start-pos end-pos state))
        (cond
         [pos2 (values pos pos2)]
         [(if (integer? end-pos)
              (pos . < . end-pos)
              (lazy-bytes-before-end? in pos end-pos))
          (define pos2 (add1 pos))
          (unless (bytes? in)
            (lazy-bytes-advance! in pos2 #f))
          (loop pos2)]
         [else (values #f #f)])]))]))

(define (check-must-string must-string in pos end-pos)
  (cond
   [(not must-string) #t]
   [(not (bytes? in)) #t]
   [(= 1 (bytes-length must-string))
    (define mc (bytes-ref must-string 0))
    (for/or ([c (in-bytes in pos end-pos)])
      (= c mc))]
   [else
    (for/or ([i (in-range pos (- end-pos (sub1 (bytes-length must-string))))])
      (for/and ([c (in-bytes in i)]
                [mc (in-bytes must-string)])
        (= c mc)))]))

(define (check-range who what in pos start-pos)
  (define len (cond
               [(bytes? in) (bytes-length in)]
               [(string? in) (string-length in)]
               [else +inf.0]))
  (unless (pos . >= . start-pos)
    (raise-arguments-error who
                           (format "~a is smaller than starting index" what)
                           what pos
                           "starting index" start-pos))
  (unless (pos . <= . len)
    (raise-arguments-error who
                           (format "~a is out of range" what)
                           what pos)))

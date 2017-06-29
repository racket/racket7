#lang racket/base
(require "../common/check.rkt"
         "../host/evt.rkt"
         "input-port.rkt"
         "output-port.rkt"
         "pipe.rkt"
         "peek-via-read-port.rkt"
         "buffer-mode.rkt")

(provide make-input-port)

(define/who (make-input-port name
                             user-read-in
                             user-peek-in
                             user-close
                             [user-get-progress-evt #f]
                             [user-commit #f]
                             [user-get-location #f]
                             [user-count-lines! void]
                             [user-init-position 1]
                             [user-buffer-mode #f])
  (check who
         (lambda (p) (or (input-port? p) (and (procedure? p) (procedure-arity-includes? p 1))))
         #:contract "(or/c (procedure-arity-includes/c 1) input-port?)"
         user-read-in)
  (check who
         (lambda (p) (or (not p) (input-port? p) (and (procedure? p) (procedure-arity-includes? p 3))))
         #:contract "(or/c (procedure-arity-includes/c 3) input-port? #f)"
         user-peek-in)
  (check who (procedure-arity-includes/c 0) user-close)
  (check who (procedure-arity-includes/c 0) #:or-false user-get-progress-evt)
  (check who (procedure-arity-includes/c 3) #:or-false user-commit)
  (check who (procedure-arity-includes/c 0) #:or-false user-get-location)
  (check who (procedure-arity-includes/c 0) #:or-false user-count-lines!)
  (check who (lambda (p) (or (exact-positive-integer? p)
                             (input-port? p)
                             (output-port? p)
                             (not p)
                             (and (procedure? p) (procedure-arity-includes? p 0))))
         #:contract "(or/c exact-positive-integer? port? #f (procedure-arity-includes/c 0))"
         user-init-position)
  (check who (lambda (p) (or (not p)
                             (and (procedure? p)
                                  (procedure-arity-includes? p 0)
                                  (procedure-arity-includes? p 1))))
         #:contract (string-append "(or/c #f (and/c (procedure-arity-includes/c 0)\n"
                                   "                (procedure-arity-includes/c 1)))")
         user-buffer-mode)

  (when (not (eqv? (input-port? user-read-in) (input-port? user-peek-in)))
    (raise-arguments-error who (if (input-port? user-read-in)
                                   "read argument is an input port, but peek argument is not a port"
                                   "read argument is not an input port, but peek argument is a port")
                          "read argument" user-read-in
                          "peek argument" user-peek-in))

  (when (and (not user-peek-in) user-get-progress-evt)
    (raise-arguments-error who "peek argument is #f, but progress-evt argument is not"
                           "progress-evt argument" user-get-progress-evt))

  (when (and (not user-get-progress-evt) user-commit)
    (raise-arguments-error who "progress-evt argument is #f, but commit argument is not"
                           "commit argument" user-commit))
  (when (and (not user-commit) user-get-progress-evt)
    (raise-arguments-error who "commit argument is #f, but progress-evt argument is not"
                           "progress-evt argument" user-get-progress-evt))
  
  (define input-pipe #f) ; `user-read-in` can redirect input
  
  (define (protect-in dest-bstr dest-start dest-end copy? read-in)
    ;; We don't trust `read-in` to refrain from modifying its
    ;; byte-string argument after it returns, and the `read-in`
    ;; interface doesn't deal with start and end positions, so copy`
    ;; dest-bstr` if needed
    (define len (- dest-end dest-start))
    (define user-bstr
      (if (or copy?
              (not (zero? dest-start))
              (not (= len dest-end)))
          (make-bytes len)
          dest-bstr))
    (define n (read-in user-bstr))
    (when (exact-positive-integer? n)
      (unless (eq? user-bstr dest-bstr)
        (bytes-copy! dest-bstr dest-start user-bstr 0 len)))
    n)

  ;; in atomic mode
  (define (check-read-result who r dest-start dest-end #:peek? [peek? #f] #:ok-false? [ok-false? #f])
    (cond
      [(exact-nonnegative-integer? r)
       (unless (r . <= . (- dest-end dest-start))
         (end-atomic)
         (raise-arguments-error who "result integer is larger than the supplied byte string"
                                "result" r
                                "byte-string length" (- dest-end dest-start)))]
      [(eof-object? r) (void)]
      [(and (procedure? r) (procedure-arity-includes? r 4))
       (unless user-peek-in
         (end-atomic)
         (raise-result-error who
                             (string-append "the port has no specific peek procedure, so"
                                            " a special read result is not allowed")
                             "special result" r))]
      [(pipe-input-port? r)
       (set! input-pipe r)]
      [(evt? r) r]
      [(and peek? (not r))
       (unless ok-false?
         (end-atomic)
         (raise-arguments-error who "returned #f when no progress evt was supplied"))]
      [else
       (end-atomic)
       (raise-result-error who
                           (string-append
                            "(or/c exact-nonnegative-integer? eof-object? evt? pipe-input-port?"
                            (if (and peek? ok-false?)
                                " #f"
                                "")
                            (if user-peek-in
                                " (procedure-arity-includes/c 4)"
                                "")
                            ")")
                           r)]))

  ;; in atomic mode
  (define (read-in dest-bstr dest-start dest-end copy?)
    (cond
      [input-pipe
       (cond
         [(zero? (pipe-content-length input-pipe))
          (set! input-pipe #f)
          (read-in dest-bstr dest-start dest-end copy?)]
         [else
          ((core-input-port-read-in input-pipe) dest-bstr dest-start dest-end copy?)])]
      [else
       (end-atomic)
       (define r (protect-in dest-bstr dest-start dest-end copy? user-read-in))
       (check-read-result '|user port read| r dest-start dest-end)
       (cond
         [input-pipe
          (start-atomic)
          (read-in dest-bstr dest-start dest-end copy?)]
         [else
          (start-atomic)
           r])]))

  ;; Used only if `user-peek-in` is a function:
  (define (peek-in dest-bstr dest-start dest-end skip-k progress-evt copy?)
    (cond
      [input-pipe
       (cond
         [((pipe-content-length input-pipe) . < . skip-k)
          (set! input-pipe #f)
          (peek-in dest-bstr dest-start dest-end skip-k copy?)]
         [else
          ((core-input-port-peek-in input-pipe) dest-bstr dest-start dest-end skip-k copy?)])]
      [else
       (end-atomic)
       (define r (protect-in dest-bstr dest-start dest-end copy?
                             (lambda (user-bstr) (user-peek-in user-bstr skip-k progress-evt))))
       (check-read-result '|user port peek| r dest-start dest-end #:peek? #t #:ok-false? progress-evt)
       (cond
         [input-pipe
          (start-atomic)
          (peek-in dest-bstr dest-start dest-end skip-k copy?)]
         [else
          (start-atomic)
          r])]))

  ;; in atomic mode
  (define (close)
    (end-atomic)
    (user-close)
    (start-atomic))

  (define (get-progress-evt)
    (define r (user-get-progress-evt))
    (unless (evt? r)
      (raise-result-error '|user port progress-evt| "evt?" r))
    r)

  ;; in atomic mode
  (define (commit amt evt ext-evt)
    (end-atomic)
    (define r (user-commit amt evt ext-evt))
    (start-atomic)
    (cond
      [(not r) #f]
      [(bytes? r) r]
      [else (make-bytes amt #\x)]))

  (define (get-location)
    (call-with-values
     (lambda () (user-get-location))
     (case-lambda
       [(line col pos) 
        (unless (or (not line) (exact-positive-integer? line))
          (raise-result-error '|user port get-location| "(or/c #f exact-positive-integer?)" line))
        (unless (or (not line) (exact-nonnegative-integer? col))
          (raise-result-error '|user port get-location| "(or/c #f exact-nonnegative-integer?)" col))
        (unless (or (not line) (exact-positive-integer? pos))
          (raise-result-error '|user port get-location| "(or/c #f exact-positive-integer?)" pos))
        (values line col pos)]
       [args
        (apply raise-arity-error '|user port get-location return| 3 args)])))

  (define init-offset
    (if (or (procedure? user-init-position)
            (input-port? user-init-position))
        #f
        (sub1 user-init-position)))

  (define file-position
    (cond
      [(input-port? user-init-position) user-init-position]
      [(output-port? user-init-position) user-init-position]
      [(procedure? user-init-position)
       (lambda ()
         (define pos (user-init-position))
         (unless (or (not pos) (exact-positive-integer? pos))
           (raise-result-error '|user port init-position| "(or/c exact-positive-integer? #f)" pos))
         (and pos (sub1 pos)))]
      [else #f]))

  (define buffer-mode
    (case-lambda
      [()
       (define m (user-buffer-mode))
       (if (or (not m) (eq? m 'block) (eq? m 'none))
           m
           (raise-result-error '|user port buffer-mode| "(or/c #f 'block 'none)" m))]
      [(m)
       (user-buffer-mode m)]))

  (cond
   [user-peek-in
    (make-core-input-port
     #:name name
     #:read-in 
     (if (input-port? user-read-in)
         user-read-in
         read-in)
     #:peek-in
     (if (input-port? user-peek-in)
         user-peek-in
         peek-in)
     #:close close
     #:get-progress-evt (and user-get-progress-evt get-progress-evt)
     #:commit (and user-commit commit)
     #:get-location (and user-get-location get-location)
     #:count-lines! user-count-lines!
     #:init-offset init-offset
     #:file-position file-position
     #:buffer-mode buffer-mode)]
   [else
    (define-values (port buffer-flusher)
      (open-input-peek-via-read
       #:name name
       #:read-in read-in
       #:close close
       #:get-location (and user-get-location get-location)
       #:count-lines! user-count-lines!
       #:init-offset init-offset
       #:file-position file-position
       #:alt-buffer-mode buffer-mode))
    port]))

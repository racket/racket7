#lang racket/base

(provide performance-region)

;; The expression form
;;
;;   (performance-region [key-expr ...] body ....)
;;
;; records the time of `body ...` and associated it with
;; the path `(list key-expr ...)`. Times for a path
;; are included in the times for the path's prefixes, but
;; not for any other path. When regions that are nested
;; dynamically, time accumlates only for the most nested
;; region.
;;
;; For example,
;;
;;    (performance-region
;;     ['compile 'module]
;;     (do-expand-module))
;;
;; counts the time for `(do-expand-module)` to '(compile) and
;; to '(compile module), and not to any other path, even if
;; the compilation occurs while expanding another module.
;;
;; The key '_ as a path element is special: it is replaced
;; by the correspondig element of the enclosing region's
;; path (if any).
;;
;; Beware that `body ...` is not in tail position when
;; performance measurement is enabled.

;; ----------------------------------------
;; Re-export this submodule to enable performance measurements

(module measure-mode racket/base
  (provide performance-region)
  
  (define-syntax-rule (performance-region [tag0-expr tag-expr ...] body ...)
    (begin
      (start-performance-region tag0-expr tag-expr ...)
      (begin0
       (let () body ...)
       (end-performance-region))))
  
  (define region-stack #f)
  (define accums (make-hasheq))

  (struct region (path
                  [start #:mutable]        ; start time
                  [as-nested #:mutable]))  ; time accumulated for nested regions
  (struct stat ([msecs #:mutable] [count #:mutable]))

  (define stat-key (gensym))

  (define-logger performance)

  (define (start-performance-region . path)
    (set! region-stack (cons (region path 
                                     (current-inexact-milliseconds)
                                     0.0)
                             region-stack)))
    
  (define (end-performance-region)
    (define now (current-inexact-milliseconds))
    (define r (car region-stack))
    (set! region-stack (cdr region-stack))

    (define full-delta (- now (region-start r)))
    (define delta (- full-delta (region-as-nested r)))

    (let loop ([accums accums] [path (region-path r)] [enclosing-path (or (and region-stack
                                                                               (region-path (car region-stack)))
                                                                          null)])
      (define key (if (and (eq? '_ (car path))
                           (pair? enclosing-path))
                      (car enclosing-path)
                      (car path)))
      (let ([accum (or (hash-ref accums key #f)
                       (let ([accum (make-hasheq)])
                         (hash-set! accums key accum)
                         accum))])
        (define s (or (hash-ref accum stat-key #f)
                      (let ([s (stat 0.0 0)])
                        (hash-set! accum stat-key s)
                        s)))
        (set-stat-msecs! s (+ delta (stat-msecs s)))
        (set-stat-count! s (add1 (stat-count s)))
        (unless (null? (cdr path))
          (loop accum (cdr path) (if (null? enclosing-path) null (cdr enclosing-path))))))
    
    (when region-stack
      (set-region-as-nested! (car region-stack)
                             (+ (region-as-nested (car region-stack))
                                full-delta))))
  
  (void (plumber-add-flush! (current-plumber)
                            (lambda (h)
                              (define (whole-len s)
                                (caar (or (regexp-match-positions #rx"[.]" s) '(0))))
                              (define-values (label-max-len value-max-len count-max-len)
                                (let loop ([accums accums] [label-len 0] [value-len 0] [count-len 0] [indent 2])
                                  (for/fold ([label-len label-len] [value-len value-len] [count-len count-len]) ([(k v) (in-hash accums)])
                                    (cond
                                     [(eq? k stat-key)
                                      (values label-len
                                              (max value-len (whole-len (format "~a" (stat-msecs v))))
                                              (max count-len (string-length (format "~a" (stat-count v)))))]
                                     [else (loop v
                                                 (max label-len (+ indent (string-length (format "~a" k))))
                                                 value-len
                                                 count-len
                                                 (+ 2 indent))]))))
                              (log-performance-info "REGION   ~a   MSECS~aCOUNT"
                                                    (make-string (max 0 (- (+ label-max-len value-max-len) 10))
                                                                 #\space)
                                                    (make-string count-max-len
                                                                 #\space))
                              (let loop ([name #f] [accums accums] [indent ""] [newline? #t])
                                (when name
                                  (define v (hash-ref accums stat-key))
                                  (log-performance-info "~a~a ~a   ~a     ~a~a"
                                                        indent
                                                        name
                                                        (make-string (+ (- label-max-len (string-length (format "~a" name)) (string-length indent))
                                                                        (- value-max-len (whole-len (format "~a" (stat-msecs v)))))
                                                                     #\space)
                                                        (regexp-replace #rx"[.](..).*" (format "~a00" (stat-msecs v)) ".\\1")
                                                        (make-string (- count-max-len (string-length (format "~a" (stat-count v))))
                                                                     #\space)
                                                        (stat-count v)))
                                (define keys (sort (for/list ([k (in-hash-keys accums)] #:when (not (eq? k stat-key))) k)
                                                   >
                                                   #:key (lambda (key) (stat-msecs (hash-ref (hash-ref accums key) stat-key)))))
                                (for ([k (in-list keys)]
                                      [i (in-naturals)])
                                  (when (and newline? (positive? i)) (log-performance-info ""))
                                  (loop k (hash-ref accums k) (string-append indent "  ") #f)))))))

;; ----------------------------------------
;; Re-export this submodule to disable measurements

(module no-measure-mode racket/base
  (provide performance-region)
  
  (define-syntax-rule (performance-region [tag0-expr tag-expr ...] body ...)
    (let () body ...)))


(require (submod "." no-measure-mode))

#lang racket/base
(require racket/list
         racket/port
         "match.rkt"
         "unique.rkt"
         "sort.rkt"
         "id.rkt"
         "vehicle.rkt"
         "top-name.rkt"
         "prim-name.rkt"
         "function.rkt"
         "state.rkt"
         "generate.rkt"
         "lambda.rkt"
         "struct.rkt"
         "union.rkt")

(module+ main
  (require racket/cmdline)
  (command-line
   #:args
   (in-file out-file)
   (define in
     (call-with-input-file*
      in-file
      (lambda (in) (for/list ([e (in-port read in)])
                     e))))
   (define orig-out (current-output-port))
   (with-output-to-file
     out-file
     #:exists 'truncate/replace
     (lambda ()
       (to-c (car in) `(begin . ,(cdr in))
             #:orig-out orig-out)))))

;; ----------------------------------------

(define (to-c exports in-e
              #:orig-out orig-out)
  (generate-header)

  ;; Make sure all names are unique:
  (define e (re-unique in-e))

  ;; All `define`d names and `let[rec[*]]` names that are flattend
  ;; into the top sequence:
  (define top-names (extract-top-names #hasheq() e))

  ;; Find all `lambda`s and `case-lambda`s, mapping each to
  ;; a newly synthesized name:
  (define lambdas (make-hasheq))
  (extract-lambdas! lambdas e)

  (define struct-knowns (extract-structs e))

  ;; Find all functions that do not need to be kept in a closure.
  ;; Top-level functions and functions bound with `letrec` are in this
  ;; category:
  (define functions (extract-functions #hasheq() e lambdas))
  (for ([(id f) (in-sorted-hash functions symbol<?)])
    (define e (function-e f))
    (hash-set! lambdas e (make-lam id e #:can-call-direct? #t)))

  (define knowns (hash-union struct-knowns functions))

  ;; Find all the primitives that we'll need to call:
  (define prim-names (extract-prim-names #hasheq() e #hasheq() top-names))

  ;; Maps variables names to
  ;;   - 'mutated
  ;;   - an integer for the use cuont
  ;; and maps `lam` record for union-find of functions
  ;; that tail-call each other
  (define state (make-hasheq))

  ;; Find mutable variables, which will need to be boxed:
  (extract-state! state e)

  ;; Generate top-level sequence just to set free-variable lists and
  ;; other state for each lambda:
  (parameterize ([current-output-port (open-output-nowhere)])
    (define vehicles (for/list ([lam (in-sorted-hash-values lambdas (compare symbol<? lam-id))])
                       (lam-vehicle lam)))
    (generate-tops e exports knowns top-names state lambdas prim-names)
    (generate-vehicles vehicles lambdas knowns top-names state)
    (hash-set! state '#:done? #t)
    (reset-genid-counters! '(__args)))

  ;; Now we know if a function that isn't already in `functions`
  ;; has zero free variables. If so, effectively lift it to
  ;; allocate the closure once
  (define closed-anonymous-functions
    (for/hash ([lam (in-hash-values lambdas)]
               #:when (and (null? (lam-free-vars lam))
                           (not (hash-ref functions (lam-id lam) #f))
                           (lam-under-lambda? lam)
                           (not (lam-unused? lam))))
      (set-lam-moved-to-top?! lam #t)
      (values (lam-id lam) lam)))

  ;; Generate prim and top-variable records:
  (generate-struct "prims" prim-names)
  (generate-struct "top" (hash-union (hash-union top-names functions)
                                     closed-anonymous-functions))
  
  (define vehicles (merge-vehicles! lambdas state orig-out))

  ;; Generate all the lambda bodies:
  (generate-prototypes vehicles)
  (generate-vehicles vehicles lambdas knowns top-names state)

  ;; Generate top-level sequence, this time to output:
  (hash-set! state '#:tops? #t)
  (generate-tops e exports knowns top-names state lambdas prim-names))

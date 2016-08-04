#lang racket/base
(require "../common/prefab.rkt")

(provide datum-map
         datum-has-elements?)

;; `(datum-map v f)` walks over `v`, traversing objects that
;; `datum->syntax` traverses to convert context to syntax objects.
;; 
;;  `(f tail? d)` is called to each datum `d`, where `tail?`
;;  indicates that the value is a pair/null in a `cdr` --- so that it
;;  doesn't need to be wrapped for `datum->syntax`, for example
;;
;; If a `seen` argument is provided, then it should be an `eq?`-based
;; hash table, and cycle checking is enabled; when a cycle is
;; discovered, the procedure attached to 'cycle-fail in the initial
;; table is called

(define (datum-map s f [seen #f])
  (let loop ([tail? #f] [s s] [prev-depth 0] [prev-seen seen])
    (define depth (add1 prev-depth)) ; avoid cycle-checking overhead for shallow cases
    (define seen
      (cond
       [(and prev-seen (depth . > . 32) (datum-has-elements? s))
        (cond
         [(hash-ref prev-seen s #f)
          ((hash-ref prev-seen 'cycle-fail) s)]
         [else (hash-set prev-seen s #t)])]
       [else prev-seen]))
    (cond
     [(null? s) (f tail? s)]
     [(pair? s)
      (f tail? (cons (loop #f (car s) depth seen)
                     (loop #t (cdr s) depth seen)))]
     [(or (symbol? s) (boolean? s) (number? s))
      (f #f s)]
     [(vector? s)
      (f #f (vector->immutable-vector
             (for/vector #:length (vector-length s) ([e (in-vector s)])
                         (loop #f e depth seen))))]
     [(box? s)
      (f #f (box-immutable (loop #f (unbox s) depth seen)))]
     [(immutable-prefab-struct-key s)
      => (lambda (key)
           (f #f
              (apply make-prefab-struct
                     key
                     (for/list ([e (in-vector (struct->vector s) 1)])
                       (loop #f e depth seen)))))]
     [(and (hash? s) (immutable? s))
      (cond
       [(hash-eq? s)
        (f #f
           (for/hasheq ([(k v) (in-hash s)])
             (values k (loop #f v depth seen))))]
       [(hash-eqv? s)
        (f #f
           (for/hasheqv ([(k v) (in-hash s)])
             (values k (loop #f v depth seen))))]
       [else
        (f #f
           (for/hash ([(k v) (in-hash s)])
             (values k (loop #f v depth seen))))])]
     [else (f #f s)])))

(define (datum-has-elements? d)
  (or (pair? d)
      (vector? d)
      (box? d)
      (immutable-prefab-struct-key d)
      (and (hash? d) (immutable? d) (positive? (hash-count d)))))

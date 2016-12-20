#lang racket/base
(require "syntax.rkt"
         "tamper.rkt"
         "../common/set.rkt")

(provide propagate-taint!
         taint-content
         
         syntax-tainted?
         syntax-clean?
         syntax-arm
         syntax-disarm
         syntax-rearm
         syntax-taint)

(define (propagate-taint! s)
  (when (eq? (syntax-tamper s) 'tainted/need-propagate)
    (set-syntax-content! s (taint-content (syntax-content s)))
    (set-syntax-tamper! s 'tainted)))

(define (taint-content d)
  (non-syntax-map d
                  (lambda (tail? x) x)
                  (lambda (sub-s)
                    (cond
                     [(tamper-tainted? (syntax-tamper sub-s)) sub-s]
                     [else (struct-copy syntax sub-s
                                        [tamper (tamper-tainted-for-content (syntax-content sub-s))])]))))

(define (syntax-tainted? s)
  (tamper-tainted? (syntax-tamper s)))

(define (syntax-clean? s)
  (tamper-clean? (syntax-tamper s)))

(define (syntax-arm s insp)
  (define t (syntax-tamper s))
  (cond
   [(tamper-tainted? t) s]
   [(and t
         (or (set-member? t insp)
             (for/or ([already-insp (in-set t)])
               (inspector-superior-or-same? already-insp insp))))
    s]
   [else
    (struct-copy syntax s
                 [tamper (set-add
                          (if t
                              (remove-inferior t insp)
                              (seteq))
                          insp)])]))


(define (remove-inferior t insp)
  (for/seteq ([already-insp (in-set t)]
              #:unless (inspector-superior-or-same? insp already-insp))
             already-insp))

(define (syntax-disarm s
                       [insp #f]) ; #f => superior to all inspectors
  (define t (syntax-tamper s))
  (cond
   [(not (tamper-armed? t)) s]
   [(not insp)
    (struct-copy syntax s
                 [tamper #f])]
   [else      
    (define new-t (remove-inferior t insp))
    (struct-copy syntax s
                 [tamper (and (not (set-empty? new-t))
                              new-t)])]))

(define (syntax-rearm s from-s)
  (define t (syntax-tamper s))
  (cond
   [(tamper-tainted? t) s]
   [else
    (define from-t (syntax-tamper from-s))
    (cond
     [(tamper-clean? from-t) s]
     [(tamper-tainted? from-t)
      (struct-copy syntax s
                   [tamper (tamper-tainted-for-content (syntax-content s))])]
     [(tamper-clean? t)
      (struct-copy syntax s
                   [tamper from-t])]
     [else
      (struct-copy syntax s
                   [tamper (for/fold ([t t]) ([from-i (in-set from-t)])
                             (cond
                              [(set-member? t from-i) t]
                              [(any-superior? t from-i) t]
                              [else (set-add (remove-inferior t from-i)
                                             from-i)]))])])]))

(define (syntax-taint s)
  (if (tamper-tainted? (syntax-tamper s))
      s
      (struct-copy syntax s
                   [tamper (tamper-tainted-for-content (syntax-content s))])))

;; ----------------------------------------

(define (any-superior? t from-i)
  (for/or ([i (in-set t)])
    (inspector-superior-or-same? i from-i)))

(define (inspector-superior-or-same? sup-i i)
  (or (eq? sup-i i)
      (inspector-superior? sup-i i)))

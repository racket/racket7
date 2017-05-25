
;; A wrapper to hide the pairness of ephemeron pairs:
(define-record-type (ephemeron create-ephemeron ephemeron?)
  (fields p))

(define (make-ephemeron key val)
  (create-ephemeron (ephemeron-cons key val)))

(define (ephemeron-value e)
  (unless (ephemeron? e)
    (raise-argument-error 'ephemeron-value "ephemeron?" e))
  (let ([v (cdr (ephemeron-p e))])
    (if (eq? v #!bwp)
        #f
        v)))

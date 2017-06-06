
(define-struct srcloc (source line column position span)
  :guard (lambda (source line column position span who)
           (check who exact-positive-integer? :or-false line)
           (check who exact-nonnegative-integer? :or-false column)
           (check who exact-positive-integer? :or-false position)
           (check who exact-nonnegative-integer? :or-false span)
           (values source line column position span)))

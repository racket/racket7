
(define-record-type (will-executor create-will-executor will-executor?)
  (fields guardian))

(define (make-will-executor)
  (create-will-executor (make-guardian)))

(define/who (will-register executor v proc)
  (check who will-executor? executor)
  ((will-executor-guardian executor) v (cons v proc)))

(define/who will-try-execute
  (case-lambda
   [(executor) (will-try-execute executor #f)]
   [(executor default)
    (check who will-executor? executor)
    (let ([v ((will-executor-guardian executor))])
      (if v
          ((cdr v) (car v))
          default))]))


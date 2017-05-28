
(define-record-type (will-executor create-will-executor will-executor?)
  (fields guardian))

(define (make-will-executor)
  (create-will-executor (make-guardian)))

(define (will-register executor v proc)
  (unless (will-executor? executor)
    (raise-argument-error 'will-register "will-executor?" executor))
  ((will-executor-guardian executor) v (cons v proc)))

(define will-try-execute
  (case-lambda
   [(executor) (will-try-execute executor #f)]
   [(executor default)
    (unless (will-executor? executor)
      (raise-argument-error 'will-try-executor "will-executor?" executor))
    (let ([v ((will-executor-guardian executor))])
      (if v
          ((cdr v) (car v))
          default))]))


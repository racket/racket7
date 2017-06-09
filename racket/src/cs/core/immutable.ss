
(define (immutable? v)
  (or (intmap? v)
      (immutable-string? v)
      (immutable-bytevector? v)
      (immutable-vector? v)
      (immutable-box? v)))


(define (immutable? v)
  (or (hamt? v)
      (immutable-string? v)
      (immutable-bytevector? v)
      (immutable-vector? v)
      (immutable-box? v)))

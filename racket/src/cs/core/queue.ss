
(define-record-type (queue* make-queue* queue?)
  (fields (mutable start) (mutable end)))

(define-record-type (node make-node node?)
  (fields elem (mutable prev) (mutable next)))

(define (make-queue)
  (make-queue* #f #f))

(define (queue-empty? q)
  (not (queue*-start q)))

(define (queue-remove! q)
  (define qs (queue*-start q))
  (cond
   [(not qs) #f]
   [else
    (let ([n (node-next qs)])
      (queue*-start-set! q n)
      (if n
	  (node-prev-set! n #f)
	  (queue*-end-set! q #f))
      (node-elem qs))]))

(define (queue-remove-end! q)
  (define qe (queue*-end q))
  (cond
   [(not qe) #f]
   [else
    (let ([n (node-prev qe)])
      (queue*-end-set! q n)
      (if n
	  (node-next-set! n #f)
	  (queue*-start-set! q #f))
      (node-elem qe))]))
	

(define (queue-fremove! q pred)
  (let loop ([qs (queue*-start q)])
    (cond
     [qs
      (let ([w (node-elem qs)])
	(cond
	 [(pred w)
	  (queue-remove-node! q qs)
	  w]
	 [else
	  (loop (node-next qs))]))]
     [else #f])))

(define (queue-remove-all! q proc)
  (let loop ([qs (queue*-start q)])
    (when qs
      (proc (node-elem qs))
      (loop (node-next qs))))
  (queue*-start-set! q #f)
  (queue*-end-set! q #f))

(define (queue-add! q w)
  (define e (queue*-end q))
  (define n (make-node w e #f))
  (cond
   [(not e)
    (queue*-start-set! q n)
    (queue*-end-set! q n)]
   [else
    (node-next-set! e n)
    (queue*-end-set! q n)])
  n)

(define (queue-add-front! q w)
  (define e (queue*-start q))
  (define n (make-node w #f e))
  (cond
    [(not e)
     (queue*-start-set! q n)
     (queue*-end-set! q n)]
    [else
     (node-prev-set! e n)
     (queue*-start-set! q n)])
  n)

(define (queue-remove-node! q n)
  ;; get out of line
  (if (node-prev n)
      (node-next-set! (node-prev n) (node-next n))
      (queue*-start-set! q (node-next n)))
  (if (node-next n)
      (node-prev-set! (node-next n) (node-prev n))
      (queue*-end-set! q (node-prev n))))

(define (queue-length q)
  (let loop ([n (queue*-start q)])
    (cond
     [(not n)
      0]
     [else
      (+ 1 (loop (node-next n)))])))

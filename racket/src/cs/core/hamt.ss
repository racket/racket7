;; Based on the "hamt" package by Jon Zeppieri

;; The MIT License (MIT)
;; 
;; Copyright (c) 2013 97jaz
;; 
;; Permission is hereby granted, free of charge, to any person obtaining a copy of
;; this software and associated documentation files (the "Software"), to deal in
;; the Software without restriction, including without limitation the rights to
;; use, copy, modify, merge, publish, distribute, sublicense, and/or sell copies of
;; the Software, and to permit persons to whom the Software is furnished to do so,
;; subject to the following conditions:
;; 
;; The above copyright notice and this permission notice shall be included in all
;; copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
;; IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY, FITNESS
;; FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE AUTHORS OR
;; COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY, WHETHER
;; IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM, OUT OF OR IN
;; CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN THE SOFTWARE.

(define array-length #%vector-length)
(define array-ref #%vector-ref)
(define array vector)

(define (array-replace arr idx val)
  (let* ([len (vector-length arr)]
         [new (make-vector len)])
    (let loop ([i 0])
      (cond
       [(fx= i idx)
        (#%vector-set! new i val)
        (loop (fx+ i 1))]
       [(fx< i len)
        (#%vector-set! new i (vector-ref arr i))
        (loop (fx+ i 1))]
       [else
        new]))))

(define (array-insert arr idx val)
  (let ([new (make-vector (fx+ (vector-length arr) 1))])
    (vector-copy! new 0 arr 0 idx)
    (#%vector-set! new idx val)
    (vector-copy! new (fx+ idx 1) arr idx (vector-length arr))
    new))

(define (array-remove arr idx)
  (let ([new (make-vector (fx- (vector-length arr) 1))])
    (vector-copy! new 0 arr 0 idx)
    (vector-copy! new idx arr (fx+ idx 1) (vector-length arr))
    new))

;; node types
(define-record entry (key value))
(define-record bnode (array bitmap count)) ; count includes subtrees
(define-record cnode (array hashcode))

(define ignored/hamt
  (begin
    (record-type-equal-procedure (record-type-descriptor bnode)
                                 (lambda (a b eql?)
                                   (hamt=? a b eql?)))
    (record-type-hash-procedure (record-type-descriptor bnode)
                                (lambda (a hash)
                                  (hamt-hash-code a hash)))))

;; To more compactly represent sets, special-case an entry
;; that has a #t value:
(define (entry*? e) (and e (not (bnode? e)) (not (cnode? e))))
(define (entry*-key e) (if (entry? e) (entry-key e) e))
(define (entry*-value e) (if (entry? e) (entry-value e) #t))
(define (make-entry* k v) (if (and k (eq? v #t)) k (make-entry k v)))

(define *nothing* (list '*nothing*))

;; encode equality type in outermost bnode type:
(define-record bnode/eq bnode ())
(define-record bnode/equal bnode ())
(define-record bnode/eqv bnode ())

(define (make-bnode* key= array bitmap count)
  (cond
   [(eq? key= eq?) (make-bnode/eq array bitmap count)]
   [(eq? key= eqv?) (make-bnode/eqv array bitmap count)]
   [else (make-bnode/equal array bitmap count)]))

(define empty-array (array))
(define empty-bnode (make-bnode empty-array 0 0))
(define empty-hasheq (make-bnode/eq empty-array 0 0))
(define empty-hash (make-bnode/equal empty-array 0 0))
(define empty-hasheqv (make-bnode/eqv empty-array 0 0))

(define hamt? bnode?)
(define immutable-hash? hamt?)

(define hamt-equal? bnode/equal?)
(define hamt-eqv? bnode/eqv?)
(define hamt-eq? bnode/eq?)

(define hamt-count bnode-count)

;; iterator position
(define-record hash-position (entry continue-k))

(define-syntax define-hash-constructors
  (syntax-rules ()
    [(_ vararg-constructor list-constructor make-bnode)
     (begin
       (define (vararg-constructor . kvs)
         (let loop ([kvs kvs] [h (make-bnode empty-array 0 0)])
           (cond
            [(null? kvs) h]
            [else
             (loop (cddr kvs) (hamt-set h (car kvs) (cadr kvs)))])))
       
       (define list-constructor
         (case-lambda
           [() (vararg-constructor)]
           [(assocs)
            (let loop ([h (vararg-constructor)] [assocs assocs])
              (if (null? assocs)
                  h
                  (loop (hamt-set h (caar assocs) (cdar assocs))
                        (cdr assocs))))])))]))

(define-hash-constructors hash make-immutable-hash make-bnode/equal)
(define-hash-constructors hasheqv make-immutable-hasheqv make-bnode/eqv)
(define-hash-constructors hasheq make-immutable-hasheq make-bnode/eq)

(define make-hamt make-immutable-hash)
(define make-hamteq make-immutable-hasheq)
(define make-hamteqv make-immutable-hasheqv)

(define (hamt-empty? h)
  (fx= (hamt-count h) 0))

(define (hamt-ref h key default)
  (cond
   [(bnode/eq? h)
    (node-ref h key (eq-hash-code key) eq? 0 default)]
   [(bnode/equal? h)
    (node-ref h key (equal-hash-code key) equal? 0 default)]
   [else
    (node-ref h key (eqv-hash-code key) eqv? 0 default)]))

(define (hamt-set h key val)
  (let-values ([(new-h added?)
                (cond
                 [(bnode/eq? h)
                  (node-set h key val (eq-hash-code key) eq? eq-hash-code 0)]
                 [(bnode/equal? h)
                  (node-set h key val (equal-hash-code key) equal? equal-hash-code 0)]
                 [else
                  (node-set h key val (eqv-hash-code key) eqv? eqv-hash-code 0)])])
    new-h))

(define (hamt-remove h key)
  (cond
   [(bnode/eq? h)
    (node-remove h key (eq-hash-code key) eq? 0)]
   [(bnode/equal? h)
    (node-remove h key (equal-hash-code key) equal? 0)]
   [else
    (node-remove h key (eqv-hash-code key) eqv? 0)]))

(define (hamt-map h proc)
  (hamt-fold h '() (lambda (k v acc) (cons (proc k v) acc))))

(define (hamt-keys h)
  (hamt-fold h '() (lambda (k _ acc) (cons k acc))))

(define (hamt-values h)
  (hamt-fold h '() (lambda (_ v acc) (cons v acc))))

(define (hamt->list h)
  (hamt-fold h '() (lambda (k v acc) (cons (cons k v) acc))))

(define (hamt-for-each h proc)
  (hamt-fold h (void) (lambda (k v _) (proc k v) (void))))

(define (hamt-fold h id proc)
  (node-fold h id proc))

(define (hamt-keys-subset? a b)
  (and (fx<= (bnode-count a) (bnode-count b))
       (cond
        [(bnode/eq? a)
         (node-keys-subset? a b eq? eq-hash-code 0)]
        [(bnode/equal? a)
         (node-keys-subset? a b equal? equal-hash-code 0)]
        [else
         (node-keys-subset? a b eqv? eqv-hash-code 0)])))

(define (hamt=? a b eql?)
  (and (= (bnode-count a) (bnode-count b))
       (cond
        [(bnode/eq? a)
         (node=? a b eql? eq? eq-hash-code 0)]
        [(bnode/equal? a)
         (node=? a b eql? equal? equal-hash-code 0)]
        [else
         (node=? a b eql? eqv? eqv-hash-code 0)])))

(define (hamt-hash-code a hash)
  (node-hash-code a hash 0 0))

;; generic iteration works by counting

(define (hamt-iterate-first h)
  (if (zero? (bnode-count h))
      #f
      0))

(define (hamt-iterate-next h pos)
  (let ([pos (add1 pos)])
    (if (fx= pos (bnode-count h))
        #f
        pos)))

(define (hamt-iterate-key h pos fail)
  (let ([e (node-entry-at-position h pos)])
    (if e
        (entry*-key e)
        fail)))

(define (hamt-iterate-value h pos fail)
  (let ([e (node-entry-at-position h pos)])
    (if e
        (entry*-value e)
        fail)))

(define (hamt-iterate-key+value h pos fail)
  (let ([e (node-entry-at-position h pos)])
    (if e
        (values (entry*-key e)
                (entry*-value e))
        fail)))

(define (hamt-iterate-pair h pos fail)
  (let ([e (node-entry-at-position h pos)])
    (if e
        (cons (entry*-key e)
              (entry*-value e))
        fail)))

;; "unsafe" iteration works with a record; it's unsafe only in the
;; sense that it doesn't make sure the iteration value is compatible
;; with the hash table

(define (unsafe-hamt-iterate-first h)
  (node-iterate h
                (lambda (e continue-k)
                  (make-hash-position e continue-k))
                (lambda (k) #f)))

(define (unsafe-hamt-iterate-next h pos)
  ((hash-position-continue-k pos)
   (lambda (e continue-k)
     (make-hash-position e continue-k))))

(define (unsafe-hamt-iterate-key h pos)
  (entry*-key (hash-position-entry pos)))

(define (unsafe-hamt-iterate-value h pos)
  (entry*-value (hash-position-entry pos)))

(define (unsafe-hamt-iterate-key+value h pos)
  (let ([e (hash-position-entry pos)])
    (values (entry*-key e)
            (entry*-value e))))

(define (unsafe-hamt-iterate-pair h pos)
  (let ([e (hash-position-entry pos)])
    (cons (entry*-key e)
          (entry*-value e))))

(define unsafe-immutable-hash-iterate-first unsafe-hamt-iterate-first)
(define unsafe-immutable-hash-iterate-next unsafe-hamt-iterate-next)
(define unsafe-immutable-hash-iterate-key unsafe-hamt-iterate-key)
(define unsafe-immutable-hash-iterate-value unsafe-hamt-iterate-value)
(define unsafe-immutable-hash-iterate-key+value unsafe-hamt-iterate-key+value)
(define unsafe-immutable-hash-iterate-pair unsafe-hamt-iterate-pair)

(define (node-ref node key keyhash key= shift default)
  (cond
   [(bnode? node) (bnode-ref node key keyhash key= shift default)]
   [(cnode? node) (cnode-ref node key keyhash key= default)]
   [else (error 'node-ref "[BUG] node-ref: unknown node type")]))

(define (node-set node key val keyhash key= key-num shift)
  (cond
   [(bnode? node) (bnode-set node key val keyhash key= key-num shift)]
   [(cnode? node) (cnode-set node key val keyhash key= key-num shift)]
   [else (error 'node-set "[BUG] node-set: unknown node type")]))

(define (node-remove node key keyhash key= shift)
  (cond
   [(bnode? node) (bnode-remove node key keyhash key= shift)]
   [(cnode? node) (cnode-remove node key keyhash key= shift)]
   [else (error 'node-remove "[BUG] node-remove: unknown node type")]))

(define (node-keys-subset? na nb key= key-num shift)
  (cond
   [(eq? na nb) #t]
   [(bnode? na)
    (cond
     [(bnode? nb)
      (let ([abm (bnode-bitmap na)]
            [bbm (bnode-bitmap nb)])
        (and (fx= abm (fxand abm bbm))
             (array-keys-subset? (bnode-array na) abm
                                 (bnode-array nb) bbm
                                 key= key-num shift)))]
     [(cnode? nb)
      (cond
       [(fx= 1 (array-length (bnode-array na)))
        (let ([e (array-ref (bnode-array na) 0)])
          (cond
           [(entry*? e)
            (not (eq? (cnode-ref nb (entry*-key e) (key-num (entry*-key e)) key= *nothing*)
                      *nothing*))]
           [else (node-keys-subset? e nb key= key-num (down shift))]))]
       [else #f])])]
   [(cnode? na)
    (cond
     [(cnode? nb)
      (and (= (cnode-hashcode na)
              (cnode-hashcode nb))
           (let ([aa (cnode-array na)]
                 [ab (cnode-array nb)])
             (and (fx<= (array-length aa) (array-length ab))
                  (let loop ([i (array-length aa)])
                    (cond
                     [(fx= i 0) #t]
                     [else
                      (let ([e (array-ref aa (fx1- i))])
                        (and (not (eq? (cnode-ref nb (entry*-key e) (key-num (entry*-key e)) key= *nothing*)
                                       *nothing*))
                             (loop (fx1- i))))])))))]
     [(bnode? nb)
      (let ([aa (cnode-array na)])
        (let loop ([i (array-length aa)])
          (cond
           [(fx= i 0) #t]
           [else
            (let ([e (array-ref aa (fx1- i))])
              (and (not (eq? (bnode-ref nb (entry*-key e) (key-num (entry*-key e)) key= shift *nothing*)
                             *nothing*))
                   (loop (fx1- i))))])))])]))

(define (array-keys-subset? aa abm ba bbm key= key-num shift)
  ;; This function is called only when `bbm` includes `abm`
  (let ([alen (array-length aa)])
    (let loop ([ai 0] [bi 0] [abit 0] [bbit 0])
      (cond
       [(fx= ai alen) #t]
       [(bit-set? abm abit)
        (let ([ae (array-ref aa ai)]
              [be (array-ref ba bi)])
          (and
           (cond
            [(entry*? ae)
             (cond
              [(entry*? be)
               (key= (entry*-key ae) (entry*-key be))]
              [(bnode? be)
               (not (eq? (bnode-ref be (entry*-key ae) (key-num (entry*-key ae)) key= (down shift) *nothing*)
                         *nothing*))]
              [(cnode? be)
               (not (eq? (cnode-ref be (entry*-key ae) (key-num (entry*-key ae)) key= *nothing*)
                         *nothing*))])]
            [(entry*? be) #f]
            [else
             (node-keys-subset? ae be key= key-num (down shift))])
           (loop (fx+ ai 1) (fx+ bi 1) (fx+ abit 1) (fx+ bbit 1))))]
       [(bit-set? bbm bbit)
        (loop ai (fx+ 1 bi) (fx+ abit 1) (fx+ bbit 1))]
       [else
        (loop ai bi (fx+ abit 1) (fx+ bbit 1))]))))

(define (node=? na nb eql? key= key-num shift)
  (cond
   [(eq? na nb) #t]
   [(bnode? na)
    (cond
     [(bnode? nb)
      (let ([abm (bnode-bitmap na)]
            [bbm (bnode-bitmap nb)])
        (and (= abm bbm)
             (array=? (bnode-array na) abm
                      (bnode-array nb)
                      eql?
                      key= key-num shift)))]
     [else #f])]
   [(cnode? na)
    (cond
     [(cnode? nb)
      (and (= (cnode-hashcode na)
              (cnode-hashcode nb))
           (let ([aa (cnode-array na)]
                 [ab (cnode-array nb)])
             (and (= (array-length aa) (array-length ab))
                  (let loop ([i (array-length aa)])
                    (cond
                     [(fx= i 0) #t]
                     [else
                      (let ([e (array-ref aa (fx1- i))])
                        (let ([v2 (cnode-ref nb (entry*-key e) (key-num (entry*-key e)) key= *nothing*)])
                          (and (not (eq? v2 *nothing*))
                               (eql? (entry*-value e) v2)
                               (loop (fx1- i)))))])))))]
     [else #f])]))

(define (array=? aa abm ba eql? key= key-num shift)
  ;; This function is called only when `bbm` equals `abm`
  (let ([alen (array-length aa)])
    (let loop ([ai 0] [abit 0])
      (cond
       [(fx= ai alen) #t]
       [(bit-set? abm abit)
        (let ([ae (array-ref aa ai)]
              [be (array-ref ba ai)])
          (let ([k (cond
                    [(entry*? ae)
                     (cond
                      [(entry*? be)
                       (and (key= (entry*-key ae) (entry*-key be))
                            (eql? (entry*-value ae) (entry*-value be)))]
                      [else #f])]
                    [(entry*? be) #f]
                    [else
                     (node=? ae be eql? key= key-num (down shift))])])
            (and k
                 (loop (fx+ ai 1) (fx+ abit 1)))))]
       [else
        (loop ai (fx+ abit 1))]))))

(define (node-hash-code na hash hc shift)
  (cond
   [(bnode? na)
    (let ([hc (hash-code-combine hc (bnode-bitmap na))])
      (array-hash-code (bnode-array na)
                       hash hc
                       shift))]
   [else
    ;; Hash code needs to be order-independent, so
    ;; collision nodes are a problem; simplify by just
    ;; using the hash code and hope that collisions are
    ;; rare
    (hash-code-combine hc (cnode-hashcode na))]))

(define (array-hash-code aa hash hc shift)
  ;; Only look at values in the array, since using
  ;; hamt bitmaps covers the keys
  (let ([alen (array-length aa)])
    (let loop ([ai 0] [hc hc])
      (cond
       [(fx= ai alen) hc]
       [else
        (let ([ae (array-ref aa ai)])
          (cond
           [(entry*? ae)
            (loop (fx1+ ai)
                  (hash-code-combine hc (hash (entry*-value ae))))]
           [else
            (loop (fx1+ ai)
                  (node-hash-code ae hash hc (down shift)))]))]))))

(define (node-fold n acc proc)
  (cond
   [(bnode? n) (array-fold (bnode-array n) acc proc)]
   [(cnode? n) (array-fold (cnode-array n) acc proc)]
   [else (error 'node-fold "[BUG] node-fold: unknown node type")]))

(define (array-fold arr acc proc)
  (let ([len (array-length arr)])
    (let loop ([acc acc] [i 0])
      (cond
       [(fx= i len) acc]
       [else
        (let ([x (array-ref arr i)])
          (if (entry*? x)
              (loop (proc (entry*-key x) (entry*-value x) acc) (fx1+ i))
              (loop (node-fold x acc proc) (fx1+ i))))]))))

(define (node-iterate n k done-k)
  (cond
   [(bnode? n) (array-iterate (bnode-array n) k done-k)]
   [(cnode? n) (array-iterate (cnode-array n) k done-k)]
   [else (error 'node-fold "[BUG] node-fold: unknown node type")]))

(define (array-iterate arr k done-k)
  (let ([len (array-length arr)])
    (let loop ([i 0] [k k])
      (cond
       [(fx= i len) (done-k k)]
       [else
        (let ([x (array-ref arr i)])
          (if (entry*? x)
              (k x (lambda (k) (loop (fx1+ i) k)))
              (node-iterate x k (lambda (k) (loop (fx1+ i) k)))))]))))

(define (bnode-ref node key keyhash key= shift default)
  (let ([e (bnode-array-ref node keyhash shift)])
    (cond
     [(not e) (return default)]
     [(entry*? e)
      (let ([k (entry*-key e)]
            [v (entry*-value e)])
        (cond
         [(key= key k) v]
         [else (return default)]))]
     [else (node-ref e key keyhash key= (down shift) default)])))

(define (cnode-ref node key keyhash key= default)
  (let ([e (cnode-array-ref node key keyhash key=)])
    (cond
     [(entry*? e) (entry*-value e)]
     [else (return default)])))

(define (bnode-set node key val keyhash key= key-num shift)
  (let* ([arr (bnode-array node)]
         [count (bnode-count node)]
         [bitmap (bnode-bitmap node)]
         [bit (bnode-bit keyhash shift)]
         [idx (bnode-idx bitmap bit)])
    (cond
     [(bit-set? bitmap bit)
      (let ([e (array-ref arr idx)])
        (cond
         [(entry*? e)
          (let ([k (entry*-key e)]
                [v (entry*-value e)])
            (cond
             [(key= key k)
              (if (eq? v val)
                  (values node #f)
                  (values (make-bnode* key=
                                       (array-replace arr idx (make-entry* key val))
                                       bitmap
                                       count)
                          #f))]
             
             [else
              (let ([child (make-node k v key val keyhash key= key-num (down shift))])
                (values (make-bnode* key= (array-replace arr idx child) bitmap (fx1+ count))
                        #t))]))]
         [else
          (let-values ([(new-child added?) (node-set e key val keyhash key= key-num (down shift))])
            (if (eq? new-child e)
                (values node #f)
                (values (make-bnode* key= (array-replace arr idx new-child) bitmap (if added? (fx1+ count) count))
                        added?)))]))]
     [else
      (values (make-bnode* key=
                           (array-insert arr idx (make-entry* key val))
                           (fxior bitmap (fxsll 1 bit))
                           (fx1+ count))
              #t)])))

(define (cnode-set node key val keyhash key= key-num shift)
  (let* ([arr (cnode-array node)]
         [hashcode (cnode-hashcode node)])
    (cond
     [(= hashcode keyhash)
      (let ([idx (cnode-index arr key key=)])
        (cond
         [idx
          (values (make-cnode (array-replace arr idx (make-entry* key val)) hashcode)
                  #f)]
         [else (values (make-cnode (array-insert arr (array-length arr) (make-entry* key val)) hashcode)
                       #t)]))]
     [else
      (let*-values ([(new)        (make-bnode (array node) (fxsll 1 (bnode-bit hashcode shift)) (array-length arr))]
                    [(new added?) (node-set new key val keyhash key= key-num shift)])
        (values new added?))])))

(define (bnode-remove node key keyhash key= shift)
  (let* ([arr (bnode-array node)]
         [count (bnode-count node)]
         [bitmap (bnode-bitmap node)]
         [bit (bnode-bit keyhash shift)]
         [idx (bnode-idx bitmap bit)])
    (cond
     [(bit-set? bitmap bit)
      (let ([e (array-ref arr idx)])
        (cond
         [(entry*? e)
          (let ([k (entry*-key e)])
            (cond
             [(key= key k)
              (let ([new-arr (array-remove arr idx)])
                (cond
                 [(contract-node? new-arr shift)
                  (array-ref new-arr 0)]
                 [else
                  (make-bnode* key= new-arr (fxxor bitmap (fxsll 1 bit)) (fx1- count))]))]
             [else
              node]))]
         [else
          (let* ([child e]
                 [new-child (node-remove child key keyhash key= (down shift))])
            (cond
             [(eq? child new-child)
              node]
             [else
              (let ([new-arr (array-replace arr idx new-child)])
                (cond
                 [(contract-node? new-arr shift)
                  (array-ref new-arr 0)]
                 [else
                  (make-bnode* key= new-arr bitmap (fx1- count))]))]))]))]
     [else node])))

(define (cnode-remove node key keyhash key= shift)
  (let ([arr (cnode-array node)]
        [hashcode (cnode-hashcode node)])
    (cond
     [(= hashcode keyhash)
      (let ([idx (cnode-index arr key key=)])
        (cond
         [idx
          (let ([new-arr (array-remove arr idx)])
            (cond
             [(contract-node? new-arr shift)
              (array-ref new-arr 0)]
             [else
              (make-cnode new-arr hashcode)]))]
         [else node]))]
     [else node])))

(define (cnode-array-ref node key keyhash key=)
  (let ([arr (cnode-array node)]
        [hashcode (cnode-hashcode node)])
    (and (= hashcode keyhash)
         (let ([i (cnode-index arr key key=)])
           (and i (array-ref arr i))))))

(define (cnode-index arr key key=)
  (let ([len (array-length arr)])
    (let loop ([i 0])
      (cond
       [(fx= i len) #f]
       [else
        (let ([e (array-ref arr i)])
          (if (key= key (entry*-key e))
              i
              (loop (fx1+ i))))]))))

(define (make-node k1 v1 k2 v2 k2hash key= key-num shift)
  (let ([k1hash (key-num k1)])
    (cond
     [(= k1hash k2hash)
      (make-cnode (array (make-entry* k1 v1) (make-entry* k2 v2)) k1hash)]
     [else
      (let*-values ([(n _) (node-set empty-bnode k1 v1 k1hash key= key-num shift)]
                    [(n _) (node-set n k2 v2 k2hash key= key-num shift)])
        n)])))

(define (contract-node? arr shift)
  (and (fx= (array-length arr) 1)
       (fx> shift 0)
       (entry*? (array-ref arr 0))))

(define (bnode-array-ref node keyhash shift)
  (let* ([arr (bnode-array node)]
         [bitmap (bnode-bitmap node)]
         [bit (bnode-bit keyhash shift)])
    (cond
     [(bit-set? bitmap bit)
      (array-ref arr (bnode-idx bitmap bit))]
     [else
      #f])))

(define (bnode-bit keyhash shift)
  (fxand (fxsra keyhash shift) #x0f))

(define (bnode-idx bitmap bit)
  (fxbit-count (fxand bitmap (fx- (fxsll 1 bit) 1))))

(define (node-entry-at-position n pos)
  (cond
   [(bnode? n) (array-entry-at-position (bnode-array n) pos)]
   [(cnode? n) (array-entry-at-position (cnode-array n) pos)]
   [else (error 'node-fold "[BUG] node-fold: unknown node type")]))

(define (array-entry-at-position arr pos)
  (let ([len (array-length arr)])
    (let loop ([i 0] [pos pos])
      (cond
       [(fx= i len) #f]
       [else
        (let ([x (array-ref arr i)])
          (cond
           [(entry*? x)
            (if (zero? pos)
                x
                (loop (fx1+ i) (fx1- pos)))]
           [(bnode? x)
            (let ([count (bnode-count x)])
              (if (fx< pos count)
                  (node-entry-at-position x pos)
                  (loop (fx1+ i) (fx- pos count))))]
           [(cnode? x)
            (let ([count (array-length (cnode-array x))])
              (if (fx< pos count)
                  (node-entry-at-position x pos)
                  (loop (fx1+ i) (fx- pos count))))]))]))))

(define (bit-set? bitmap bit)
  (fxlogbit? bit bitmap))

(define (down shift)
  (fx+ shift 4))

(define (return default)
  (if (procedure? default)
      (default)
      default))

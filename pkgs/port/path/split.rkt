#lang racket/base
(require "../error/abort.rkt"
         "path.rkt"
         "check-path.rkt"
         "sep.rkt"
         "drive-letter.rkt"
         "cleanse.rkt")

(provide split-path
         explode-path)

(define (split-path p)
  (check-path-argument 'split-path p)
  (split (->path p)))

(define (explode-path p)
  (check-path-argument 'explode-path p)
  (reverse (split (->path p) #:explode? #t)))

;; ----------------------------------------

(define (split p #:explode? [explode? #f])
  (cond
   [(not (eq? (path-convention p) 'windows))
    (split-after-drive p #:explode? explode?)]
   [else
    ;; Look for a Windows drive spec, then (usually) continue
    ;; to `split-after-drive`:
    (define bstr (path-bytes p))
    (cond
     [(and ((bytes-length bstr) . > . 2)
           (is-sep? (bytes-ref bstr 0) 'windows)
           (is-sep? (bytes-ref bstr 1) 'windows))
      (define //?-drive-end (parse-//?-drive bstr))
      (cond
       [//?-drive-end
        (define allow-double-before //?-drive-end)
        (cond
         [(eq? //?-drive-end 'reld)
          ;; `\\?\REL\` or `\\?\RED\` path. Handle it directly as a special case
          (split-reld bstr //?-drive-end)]
         [else
          (split-after-drive p
                             #:drive-end (cond
                                          [(and (//?-drive-end . < . (bytes-length bstr))
                                                (eq? (bytes-ref bstr //?-drive-end) (char->integer #\\)))
                                           ;; Happens with \\?\c:\\, for example
                                           (add1 //?-drive-end)]
                                          [else //?-drive-end])
                             #:no-slash-sep? #t
                             #:no-up? #t
                             #:explode? explode?)])]
       [else
        (define //-drive-end (parse-//-drive bstr))
        (cond
         [//-drive-end
          (split-after-drive p
                             #:drive-end (cond
                                          [(and (//-drive-end . < . (bytes-length bstr))
                                                (is-sep? (bytes-ref bstr //?-drive-end) 'windows))
                                           (add1 //-drive-end)]
                                          [else //-drive-end])
                             #:allow-double-before 1
                             #:explode? explode?)]
         [else
          (split-after-drive p #:explode? explode?)])])]
     [(and ((bytes-length bstr) . > . 2)
           (drive-letter? (bytes-ref bstr 0))
           (eq? (bytes-ref bstr 1) (char->integer #\:)))
      (split-after-drive p
                         #:drive-end (cond
                                      [(and (2 . < . (bytes-length bstr))
                                            (is-sep? (bytes-ref bstr 2) 'windows))
                                       3]
                                      [else 2])
                         #:explode? explode?)]
     [(split-after-drive p #:explode? explode?)])]))

;; ----------------------------------------

;; Find a separator to split on, avoiding the Windows drive portion of
;; a path
(define (split-after-drive p
                           #:len [in-len #f]
                           #:drive-end [drive-end 0]
                           #:no-slash-sep? [no-slash-sep? #f]
                           #:no-up? [no-up? #f]
                           #:allow-double-before [allow-double-before 0]
                           #:explode? explode?)
  (define convention (path-convention p))
  ;; Consecutive slashes can cause all sorts of mischief, both for
  ;; finding a separtor and making an unintended result after splitting,
  ;; so clean them up as a first step
  (define bstr (if in-len
                   (path-bytes p)
                   (clean-double-slashes (path-bytes p) convention allow-double-before)))
  (define len (or in-len (bytes-length bstr)))
  
  (define-values (split-pos ends-sep?)
    (let loop ([i (sub1 len)] [ends-sep? #f])
      (cond
       [(i . < . drive-end) (values #f ends-sep?)]
       [else
        (define sep?
          (cond
           [no-slash-sep? (eq? (bytes-ref bstr i) #\\)]
           [else (is-sep? (bytes-ref bstr i) convention)]))
        (cond
         [sep?
          (if (i . < . (sub1 len))
              (values i ends-sep?)
              (loop (sub1 i) #t))]
         [else
          (loop (sub1 i) ends-sep?)])])))
  ;; The `split-pos` argument is #f or less than `(sub1 len)`
  
  (cond
   [(not split-pos)
    ;; No splitting available: relative or exactly a root
    (cond
     [(or (is-sep? (bytes-ref bstr 0) convention)
          (positive? drive-end))
      ;; root
      (define new-p (path (subbytes bstr 0 len) convention))
      (if explode?
          (list new-p)
          (values #f new-p #t))]
     [else
      ;; relative
      (define-values (name is-dir?) (split-tail bstr len 0
                                                convention
                                                #:ends-sep? ends-sep?
                                                #:no-up? no-up?))
      (if explode?
          (list name)
          (values 'relative name is-dir?))])]
   [else
    ;; Split at the discovered separator
    (define-values (name is-dir?) (split-tail bstr len (add1 split-pos)
                                              convention
                                              #:ends-sep? ends-sep?
                                              #:no-up? no-up?))
    (cond
     [(zero? split-pos)
      (define base (if (eq? (bytes-ref bstr 0) #\/)
                       (path #"/" convention)
                       (path (subbytes bstr 0 1) convention)))
      (cond
       [explode?
        (list name base)]
       [else
        (values base name is-dir?)])]
     [else
      (define-values (exposed-bstr exposed-len)
        (bytes->exposed-path-bytes bstr (add1 split-pos) convention #:already-protected? no-up?))
      (cond
       [explode?
        (cons name
              (split-after-drive (path exposed-bstr convention)
                                 #:explode? #t
                                 #:len exposed-len
                                 #:drive-end drive-end
                                 #:no-slash-sep? no-slash-sep?
                                 #:no-up? no-up?
                                 #:allow-double-before allow-double-before))]
       [else
        (define base (path (subbytes exposed-bstr 0 exposed-len) convention))
        (values base name is-dir?)])])]))

;; ----------------------------------------

;; Extract a name and `is-dir?` result from the end of a path:
(define (split-tail bstr len start-pos
                    convention
                    #:ends-sep? ends-sep?
                    #:no-up? no-up?)
  (cond
   ;; check for 'up
   [(and (not no-up?)
         ((+ start-pos 2) . <= . len)
         (eq? (bytes-ref bstr start-pos) (char->integer #\.))
         (eq? (bytes-ref bstr (+ start-pos 1)) (char->integer #\.))
         (or ((+ start-pos 2) . = . len)
             (and ((+ start-pos 3) . = . len)
                  ends-sep?)))
    (values 'up #t)]
   ;; check for 'same
   [(and (not no-up?)
         ((+ start-pos 1) . <= . len)
         (eq? (bytes-ref bstr start-pos) (char->integer #\.))
         (or ((+ start-pos 1) . = . len)
             (and ((+ start-pos 2) . = . len)
                  ends-sep?)))
    (values 'same #t)]
   ;; other relative
   [else
    (define new-bstr (cond
                      [ends-sep?
                       (subbytes bstr start-pos (sub1 len))]
                      [(zero? start-pos)
                       (bytes->immutable-bytes bstr)]
                      [else  
                       (subbytes bstr start-pos)]))
    (define prot-bstr (if (or no-up? ends-sep?)
                          (protect-path-element new-bstr convention)
                          new-bstr))
    (values (path prot-bstr convention)
            ends-sep?)]))

;; ----------------------------------------
;; More work to do here for Windows paths

(define (bytes->exposed-path-bytes bstr pos convention #:already-protected? already-protected?)
  (values bstr pos))

(define (protect-path-element new-bstr convention)
  new-bstr)

;; ----------------------------------------

(define (parse-//?-drive bstr) (abort "finish me"))
(define (parse-//-drive bstr) (abort "finish-me"))
(define (split-reld bstr drive-end) (abort "finish-me"))

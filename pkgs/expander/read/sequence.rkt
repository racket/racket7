#lang racket/base
(require "../common/struct-star.rkt"
         "config.rkt"
         "readtable.rkt"
         "whitespace.rkt"
         "delimiter.rkt"
         "consume.rkt"
         "closer.rkt"
         "error.rkt"
         "indentation.rkt"
         "parameter.rkt"
         "wrap.rkt")

(provide read-unwrapped-sequence)

(define (read-unwrapped-sequence read-one opener-c opener closer in seq-config
                                 #:dot-mode [dot-mode 'all]
                                 #:shape-tag? [shape-tag? #f]
                                 #:whitespace-read-one [whitespace-read-one read-one]
                                 #:first-read-one [first-read-one read-one])
  (define head #f)
  (define indentation (make-indentation closer in seq-config))
  (define config (struct*-copy read-config seq-config
                               [indentations (cons indentation
                                                   (read-config-indentations seq-config))]))

  (define (read-one/not-eof read-one)
    (define e (read-one in config))
    (when (eof-object? e)
      (reader-error in config #:eof? #t
                    "expected a `~a` to close `~a`~a"
                    (closer-name closer config)
                    opener-c
                    (indentation-possible-cause config)))
    e)

  (define seq
    (let loop ([first? #t] [first-read-one first-read-one])
      (define c (skip-whitespace-and-comments! whitespace-read-one in config))
      (define ec (effective-char c config))
      (cond
       [(eqv? ec closer)
        (consume-char in ec)
        null]
       [(and (not first?)
             (eqv? ec #\.)
             (check-parameter read-accept-dot config)
             (char-delimiter? (peek-char-or-special in 1) config))
        ;; Found a `.`: maybe improper or maybe infix
        (define-values (dot-line dot-col dot-pos) (port-next-location in))
        (consume-char in c)
        (track-indentation! config dot-line dot-col)
        
        (unless (and dot-mode
                     ;; don't allow another `.` if we've seen an infix
                     (not head))
          (reader-error in (reading-at config dot-line dot-col dot-pos)
                        "illegal use of `.`"))
        
        ;; Read one item for improper list or for infix:
        (define v (read-one/not-eof first-read-one))
        
        ;; Check for infix or list termination:
        (define rest-c (skip-whitespace-and-comments! whitespace-read-one in config))
        (define rest-ec (effective-char rest-c config))
        
        (cond
         [(eqv? rest-ec closer)
          ;; Improper list
          (consume-char in rest-c)
          v]
         [(and (eqv? rest-ec #\.)
               (check-parameter read-accept-dot config)
               (check-parameter read-accept-infix-dot config)
               (char-delimiter? (peek-char-or-special in 1) config))
          ;; Infix mode
          (set! head (box v))
          (consume-char in rest-c)
          
          (define-values (dot2-line dot2-col dot2-pos) (port-next-location in))
          (track-indentation! config dot2-line dot2-col)
          
          ;; Check for a closer right after the second dot:
          (define post-c (skip-whitespace-and-comments! whitespace-read-one in config))
          (define post-ec (effective-char post-c config))
          (when (or (eof-object? post-ec)
                    (char=? post-ec closer))
            (reader-error in (reading-at config dot-line dot-col dot-pos)
                          #:eof? (eof-object? post-ec)
                          "illegal use of `.`"))
          
          ;; No closer => another item or EOF
          (loop #f read-one)]
         [else
          ;; Something else after a single element after a single dot
          (reader-error in (reading-at config dot-line dot-col dot-pos)
                        "illegal use of `.`")])]
       [else
        (cons (read-one/not-eof first-read-one) (loop #f read-one))])))
  (define full-seq (if head
                       (cons (unbox head) seq)
                       seq))
  (if shape-tag?
      (add-shape-tag opener in config full-seq)
      full-seq))

;; ----------------------------------------

(define (add-shape-tag opener in config seq)
  (define tag
    (case opener
      [(#\[) (and (check-parameter read-square-bracket-with-tag config) '#%brackets)]
      [(#\{) (and (check-parameter read-curly-brace-with-tag config) '#%braces)]
      [else #f]))
  (if tag
      (cons (wrap tag in config #f) seq)
      seq))

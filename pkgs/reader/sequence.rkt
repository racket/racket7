#lang racket/base
(require "config.rkt"
         "readtable.rkt"
         "whitespace.rkt"
         "delimiter.rkt"
         "consume.rkt"
         "error.rkt"
         "indentation.rkt")

(provide read-unwrapped-sequence)

(define (read-unwrapped-sequence read-one opener closer in seq-config #:dot-mode [dot-mode 'all])
  (define head #f)
  (define indentation (make-indentation closer in seq-config))
  (define config (struct-copy read-config seq-config
                              [indentations (cons indentation
                                                  (read-config-indentations seq-config))]))
  (define (read-one/not-eof)
    (define e (read-one in config))
    (when (eof-object? e)
      (reader-error in config #:eof? #t
                    "expected a `~a` to close `~a`~a"
                    closer
                    opener
                    (indentation-possible-cause config)))
    e)
  (define seq
    (let loop ()
      (define c (skip-whitespace-and-comments! in config))
      (define ec (effective-char c config))
      (cond
       [(eqv? ec closer)
        (consume-char in ec)
        null]
       [(and (eqv? ec #\.)
             (char-delimiter? (peek-char in 1) config))
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
        (define v (read-one/not-eof))
        
        ;; Check for infix or list termination:
        (define rest-c (skip-whitespace-and-comments! in config))
        (define rest-ec (effective-char rest-c config))
        
        (cond
         [(eqv? rest-ec closer)
          ;; Improper list
          (consume-char in rest-c)
          v]
         [(and (eqv? rest-ec #\.)
               (char-delimiter? (peek-char in 1) config))
          ;; Infix mode
          (set! head (box v))
          (consume-char in rest-c)
          
          (define-values (dot2-line dot2-col dot2-pos) (port-next-location in))
          (track-indentation! config dot2-line dot2-col)
          
          ;; Check for a closer right after the second dot:
          (define post-c (skip-whitespace-and-comments! in config))
          (define post-ec (effective-char post-c config))
          (when (or (eof-object? post-ec)
                    (char=? post-ec closer))
            (reader-error in (reading-at config dot-line dot-col dot-pos)
                          #:eof? (eof-object? post-ec)
                          "illegal use of `.`"))
          
          ;; No closer => another item or EOF
          (loop)]
         [else
          ;; Something else after a single element after a single dot
          (reader-error in (reading-at config dot-line dot-col dot-pos)
                        "illegal use of `.`")])]
       [else
        (cons (read-one/not-eof) (loop))])))
  (if head
      (cons (unbox head) seq)
      seq))

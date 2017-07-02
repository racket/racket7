#lang racket/base
(require "../common/check.rkt"
         "../host/thread.rkt"
         "../host/rktio.rkt"
         "../string/utf-16-encode.rkt"
         "parameter.rkt"
         "string.rkt"
         "recase.rkt"
         "nul.rkt")

(provide string-locale<?
         string-locale=?
         string-locale>?
         string-locale-ci<?
         string-locale-ci=?
         string-locale-ci>?)

(define (make-string-comparsion who cmp portable-cmp ci?)
  (lambda (arg . args)
    (check who string? arg)
    (for ([arg (in-list args)])
      (check who string? arg))
    (define locale-on? (current-locale))
    (let loop ([prev arg] [args args])
      (cond
        [(null? args) #t]
        [(if locale-on?
             (cmp (collate prev (car args) ci?) 0)
             (portable-cmp prev (car args)))
         (loop (car args) (cdr args))]
        [else #f]))))

(define/who string-locale<?
  (make-string-comparsion who < string<? #f))
(define/who string-locale=?
  (make-string-comparsion who = string=? #f))
(define/who string-locale>?
  (make-string-comparsion who > string>? #f))

(define/who string-locale-ci<?
  (make-string-comparsion who < string-ci<? #t))
(define/who string-locale-ci=?
  (make-string-comparsion who = string-ci=? #t))
(define/who string-locale-ci>?
  (make-string-comparsion who > string-ci>? #t))

;; The rktio-provided string-comparison functions don't handle strings
;; that contain the nul character, and locale-specific conversion also
;; may not support nul characters. So, we handle nul ourselves,
;; imposing the rule that a string is greater than any prefix of the
;; string.
(define (collate s1 s2 ci?)
  (define full-l1 (string-length s1))
  (define full-l2 (string-length s2))
  (let loop ([i1 0] [i2 0] [l1 full-l1] [l2 full-l2])
    (define t-l1 (string-length-up-to-nul s1 i1 full-l1))
    (define t-l2 (string-length-up-to-nul s2 i2 full-l2))
    (cond
      [(and (= l1 t-l1)
            (= l2 t-l2))
       (collate/no-nul (maybe-substring s1 i1 l1) (maybe-substring s2 i2 l2) ci?)]
      [else
       (define v (collate/no-nul (substring s1 i1 t-l1)
                                 (substring s2 i2 t-l2)
                                 ci?))
       (cond
         [(not (zero? v)) v]
         [(= l1 t-l1) (if (= l2 t-l2) 0 -1)]
         [(= l2 t-l2) 1]
         [else
          ;; Both strings have more content, so skip nuls and check more
          (loop (+ i1 t-l1 1) (+ i2 t-l2 1) (- l1 t-l1 1) (- l2 t-l2 1))])])))

;; Compare two strings that do not include the nul character
(define (collate/no-nul s1 s2 ci?)
  (cond
    [(and (equal? (current-locale) "")
          (not (zero? (bitwise-and (rktio_convert_properties rktio) RKTIO_CONVERT_STRCOLL_UTF16))))
     ;; The OS provides a UTF-16-based collation function, so use that
     (define s1-16 (utf-16-encode s1))
     (define s2-16 (utf-16-encode s2))
     (rktio_strcoll_utf16 rktio
                          s1-16 (arithmetic-shift (bytes-length s1-16) -1)
                          s2-16 (arithmetic-shift (bytes-length s2-16) -1)
                          ci?)]
    [else
     (define s1-locale (string->bytes/locale s1))
     (define s2-locale (string->bytes/locale s2))
     (atomically
      (sync-locale!)
      (if ci?
          (rktio_locale_strcoll rktio
                                (locale-recase #:up? #f s1-locale)
                                (locale-recase #:up? #f s2-locale))
          (rktio_locale_strcoll rktio s1-locale s2-locale)))]))

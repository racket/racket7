#lang racket/base
(require "../string/convert.rkt"
         (rename-in "path.rkt"
                    [string->path raw:string->path])
         "check.rkt"
         "sep.rkt"
         "build.rkt"
         "split.rkt"
         "relativity.rkt"
         "cleanse.rkt"
         "simplify.rkt"
         "parameter.rkt"
         "directory-path.rkt"
         "api.rkt")

(provide (rename-out [is-path? path?])
         path-for-some-system?
         
         string->path
         path->string
         bytes->path
         path->bytes

         string->path-element
         bytes->path-element
         path-element->string
         path-element->bytes
         
         path<?
         
         path-convention-type

         build-path
         build-path/convention-type
         
         split-path
         explode-path
         
         absolute-path?
         relative-path?
         complete-path?

         current-directory
         current-drive

         path->complete-path
         path->directory-path
         
         cleanse-path
         simplify-path)

(define (string->path s)
  (check 'string->path string? s)
  (check-path-string 'string->path s)
  (raw:string->path s))

(define (path->string p)
  (check 'path->string is-path? #:contract "path?" p)
  (bytes->string/locale (path-bytes p) #\?))

(define (bytes->path bstr [convention (system-path-convention-type)])
  (check 'bytes->path bytes? bstr)
  (check-convention 'bytes->path convention)
  (check-path-bytes 'bytes->path bstr)
  (path (bytes->immutable-bytes bstr) convention))

(define (path->bytes p)
  (check 'path->bytes path? #:contract "path-for-some-system?" p)
  (bytes-copy (path-bytes p)))

(define (string->path-element s)
  (check 'string->path-element string? s)  
  (check-path-string 'string->path-element s)
  (case (system-path-convention-type)
    [(unix)
     (check-path-string 'string->path-element s)
     (when (or (equal? s "..")
               (equal? s ".")
               (for/or ([c (in-string s)])
                 (eqv? c #\/)))
       (raise-arguments-error 'string->path-element
                              "cannot be converted to a path element"
                              "path" s
                              "explanation" "path can be split, is not relative, or names a special element"))]
    [(windows)
     (error 'string->path-element "fixme")])
  (do-bytes->path-element (string->bytes/locale s #\?)
                          (system-path-convention-type)
                          'string->path-element
                          s))
                               
(define (bytes->path-element bstr [convention (system-path-convention-type)])
  (check 'bytes->path-element bytes? bstr)
  (check-convention 'bytes->path-element convention)
  (check-path-bytes 'bytes->path-element bstr)
  (do-bytes->path-element bstr
                          convention
                          'bytes->path-element
                          bstr))

(define (path-element? p)
  (cond
   [(path? p)
    (define bstr (path-bytes p))
    (define convention (path-convention p))
    (and
     ;; Quick pre-check: any separators?
     (not (for/or ([c (in-bytes bstr)]
                   [i (in-naturals)])
            (and (is-sep? c convention)
                 i)))
     (let-values ([(base name dir?) (split-path p)])
       (and (symbol? base)
            (path? name))))]
   [else #f]))

(define (do-bytes->path-element bstr convention who orig-arg)
  (define len (bytes-length bstr))
  (define p (path (bytes->immutable-bytes bstr) convention))
  (unless (path-element? p)
    (raise-arguments-error who
                           (string-append "cannot be converted to a path element;\n"
                                          " path can be split, is not relative, or names a special element")
                           "argument" orig-arg))
  p)

(define (path-element->string p)
  (check 'path-element->string path-element? p)
  (bytes->string/locale (path-bytes p) #\?))

(define (path-element->bytes p)
  (check 'path-element->string path-element? p)
  (bytes-copy (path-bytes p)))

(define path<?
  (case-lambda
    [(p)
     (check 'path<? is-path? #:contract "path?" p)
     #t]
    [(p1 p2)
     (check 'path<? is-path? #:contract "path?" p1)
     (check 'path<? is-path? #:contract "path?" p2)
     (bytes<? (path-bytes p1) (path-bytes p2))]
    [(p . ps)
     (check 'path<? is-path? #:contract "path?" p)
     (let loop ([bstr (path-bytes p)] [ps ps])
       (cond
        [(null? ps) #t]
        [else
         (define p (car ps))
         (check 'path<? is-path? #:contract "path?" p)
         (define bstr2 (path-bytes p))
         (and (bytes<? bstr bstr2)
              (loop bstr2 (cdr ps)))]))]))

(define (path-convention-type p)
  (check 'path-convention-type path-for-some-system? p)
  (path-convention p))

#lang racket/base
(require "../common/performance.rkt"
         (rename-in "../read/main.rkt" 
                    [read main:read]
                    [read-language main:read-language])
         "syntax.rkt"
         "property.rkt"
         "original.rkt"
         "../eval/dynamic-require.rkt"
         "../namespace/api-module.rkt"
         "srcloc.rkt"
         "../host/linklet.rkt")

(provide read
         read/recursive
         read-syntax
         read-syntax/recursive
         read-language)

(define (read-syntax src in)
  (read* in
         #:for-syntax? #t
         #:source src))

(define (read-syntax/recursive src in start readtable graph?)
  (read* in
         #:for-syntax? #t
         #:recursive? #t
         #:source src
         #:init-c start
         #:readtable readtable
         #:local-graph? (not graph?)))

(define (read in)
  (read* in
         #:for-syntax? #f))

(define (read/recursive in start readtable graph?)
  (read* in
         #:for-syntax? #f
         #:recursive? #t
         #:init-c start
         #:readtable readtable
         #:local-graph? (not graph?)))

(define (read* in
               #:for-syntax? for-syntax?
               #:recursive? [recursive? #f]
               #:source [source #f]
               #:init-c [init-c #f]
               #:readtable [readtable (current-readtable)]
               #:local-graph? [local-graph? #f])
  (performance-region
   ['read]
   (main:read in
              #:for-syntax? for-syntax?
              #:recursive? recursive?
              #:source source
              #:wrap (and for-syntax?
                          read-to-syntax)
              #:init-c init-c
              #:readtable readtable
              #:local-graph? local-graph?
              #:read-compiled read-compiled-linklet
              #:dynamic-require dynamic-require
              #:module-declared? read-module-declared?
              #:coerce read-coerce)))

(define (read-language in fail-thunk)
  (main:read-language in fail-thunk
                      #:for-syntax? #t
                      #:wrap read-to-syntax
                      #:read-compiled read-compiled-linklet
                      #:dynamic-require dynamic-require
                      #:module-declared? read-module-declared?
                      #:coerce read-coerce))

(define (read-to-syntax s-exp srcloc rep)
  (struct-copy syntax empty-syntax
               [content (datum-intern-literal s-exp)]
               [srcloc srcloc]
               [props (case rep
                        [(#\[) original-square-props]
                        [(#\{) original-curly-props]
                        [else original-props])]))
  
(define original-props
  (syntax-props (syntax-property empty-syntax original-property-sym #t)))
(define original-square-props
  (syntax-props (syntax-property (syntax-property empty-syntax original-property-sym #t)
                                 'paren-shape #\[)))
(define original-curly-props
  (syntax-props (syntax-property (syntax-property empty-syntax original-property-sym #t)
                                 'paren-shape #\{)))

(define (read-module-declared? mod-path)
  (module-declared? mod-path #t))

(define (read-coerce for-syntax? v srcloc)
  (cond
   [(not for-syntax?)
    (cond
     [(syntax? v) (syntax->datum v)]
     [else v])]
   [else
    (datum->syntax #f v (and srcloc (to-srcloc-stx srcloc)))]))

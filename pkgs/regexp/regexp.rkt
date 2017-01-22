#lang racket/base
(require "error.rkt"
         "parse/main.rkt"
         "validate.rkt"
         "convert.rkt"
         "compile.rkt"
         "anchor.rkt"
         "must-string.rkt")

(provide (struct-out rx:regexp)
         make-regexp
         regexp?
         byte-regexp?
         pregexp?
         byte-pregexp?)

(struct rx:regexp (bytes?   ; a bytes matcher (as opposed to string matcher)?
                   px?      ; a pregexp (as opposed to pregexp)?
                   source   ; original source string/bytes, but made immutable
                   matcher  ; compiled matcher function; see "compile.rkt"
                   num-groups ; number of `(...)` groups for reporting submatches
                   references?  ; any backreferences in the pattern?
                   max-lookbehind ; max lookbehnd
                   anchored?    ; starts with `^`?
                   must-string) ; shortcut: a byte string that must appear in a match
        #:reflection-name 'regexp
        #:property prop:custom-write (lambda (rx port mode)
                                       (write-bytes (if (rx:regexp-px? rx)
                                                        #"#px"
                                                        #"#rx")
                                                    port)
                                       (write (rx:regexp-source rx) port)))

(define (make-regexp who orig-p px? as-bytes? handler)
  (call-with-continuation-prompt
   (lambda ()
     (define p (if (bytes? orig-p)
                   (bytes->immutable-bytes orig-p)
                   (string->immutable-string orig-p)))
     (define-values (raw-rx num-groups references?) (parse p #:px? px?))
     (define rx (if as-bytes? raw-rx (convert raw-rx)))
     (define max-lookbehind (validate rx num-groups))
     (define matcher (compile rx))
     (rx:regexp as-bytes? px? p
                matcher num-groups references? max-lookbehind
                (anchored? rx) (get-must-string rx)))
   regexp-error-tag
   (lambda (str)
     (if handler
         (handler str)
         (error who "~a" str)))))

(define (regexp? v)
  (and (rx:regexp? v)
       (not (rx:regexp-bytes? v))))

(define (byte-regexp? v)
  (and (rx:regexp? v)
       (rx:regexp-bytes? v)))

(define (pregexp? v)
  (and (rx:regexp? v)
       (not (rx:regexp-bytes? v))
       (rx:regexp-px? v)))

(define (byte-pregexp? v)
  (and (rx:regexp? v)
       (rx:regexp-bytes? v)
       (rx:regexp-px? v)))

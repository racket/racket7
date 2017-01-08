#lang at-exp racket
(require "main.rkt")

(define (s->p . strs)
  (define p (open-input-string (apply string-append strs)))
  (port-count-lines! p)
  p)

(define (mrc)
  (make-read-config #:source "input"))

(read-one (s->p "#:a") (mrc))
(read-one (s->p "#\\a") (mrc))
(read-one (s->p "#\\U3BB") (mrc))
(read-one (s->p "#\\\u3BB") (mrc))
(read-one (s->p "|ap ple|Pie") (mrc))
(read-one (s->p "(a b #%c)") (mrc))
(read-one (s->p "(a b . c)") (mrc))
(read-one (s->p "(b . a #| a |# . c)") (mrc))
(read-one (s->p "(a 1.0 ; comment\n c)") (mrc))
(read-one (s->p "(a \"1.0\" c)") (mrc))
(read-one (s->p "'('a `b ,c ,@d ,@ e #'f #`g #,h #,@i)") (mrc))
(read-one (s->p "(#t)") (mrc))
(read-one (s->p "(#TRUE)") (mrc))
(read-one (s->p "(#fAlSe)") (mrc))
(read-one (s->p "#(fAl Se)") (mrc))
(read-one (s->p "#hash{(fAl . Se) (7 . 9)}") (mrc))
(read-one (s->p "{fAl Se}") (mrc))
(read-one @s->p{"apple\n\"\x30\7\07\u3BB\U1F600\uD83D\uDE00"} (mrc))
(read-one @s->p{#"apple\n\"\x30\7\07"} (mrc))
(read-one @s->p{#<<HERE
                This is text and
                HERE we go
                to finish the text
                HERE
                not included}
          (mrc))
(parameterize ([read-curly-brace-with-tag #t])
  (read-one (s->p "{fAl Se}") (mrc)))
(parameterize ([read-case-sensitive #f])
  (read-one (s->p "Case\\InSens") (mrc)))
(with-handlers ([exn:fail:read? exn-message])
  (read-one (s->p "{  fAl\n Se)") (mrc)))

(define s (format "~s" (for/list ([i 100000])
                         i)))
(void (time (read-one (s->p s) (mrc))))
(void (time (read (s->p s))))

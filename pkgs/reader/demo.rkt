#lang at-exp racket
(require (rename-in "main.rkt" [read main:read]))

(define (s->p . strs)
  (define p (open-input-string (apply string-append strs)))
  (port-count-lines! p)
  p)

(define (test-read in)
  (main:read in #:source "input"))

(test-read (s->p "#:a"))
(test-read (s->p "#\\a"))
(test-read (s->p "#\\U3BB"))
(test-read (s->p "#\\\u3BB"))
(test-read (s->p "|ap ple|Pie"))
(test-read (s->p "(a b #%c)"))
(test-read (s->p "(a #;z b . c)"))
(parameterize ([read-cdot #t])
  (test-read (s->p "(a b . c)")))
(parameterize ([read-cdot #t])
  (test-read (s->p "a.b.c.d|.|f")))
(test-read (s->p "(b . a #| a |# . c)"))
(test-read (s->p "(a 1.0 ; comment\n c)"))
(test-read (s->p "(a \"1.0\" c)"))
(test-read (s->p "'('a `b ,c ,@d ,@ e #'f #`g #,h #,@i)"))
(test-read (s->p "(#t)"))
(test-read (s->p "(#TRUE)"))
(test-read (s->p "(#fAlSe)"))
(test-read (s->p "#005(fAl Se)"))
(let ([ht (test-read (s->p "#1=#hasheq((#1# . #1#))"))])
  (hash-ref ht ht))
(test-read (s->p "#hash{(fAl . Se) (7 . 9)}"))
(test-read (s->p "#s(fAl Se)"))
(test-read (s->p "#&fox"))
(test-read @s->p{#px#"fox"})
(test-read (s->p "{fAl Se}"))
(test-read (s->p "#! ok \\\n more\n 8"))
(test-read @s->p{"apple\n\"\x30\7\07\u3BB\U1F600\uD83D\uDE00"})
(test-read @s->p{#"apple\n\"\x30\7\07"})
(test-read @s->p{#<<HERE
                This is text and
                HERE we go
                to finish the text
                HERE
                not included}
         )
(parameterize ([read-curly-brace-with-tag #t])
  (test-read (s->p "{fAl Se}")))
(parameterize ([read-case-sensitive #f])
  (test-read (s->p "Case\\InSens")))
(with-handlers ([exn:fail:read? exn-message])
  (test-read (s->p "{  fAl\n Se)")))

(define s (format "~s" (for/list ([i 100000])
                         i)))
(void (time (test-read (s->p s))))
(void (time (read (s->p s))))

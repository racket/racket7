#lang at-exp racket
(require (rename-in "main.rkt" [read main:read]))

(define (s->p . strs)
  (define p (open-input-string (apply string-append strs)))
  (port-count-lines! p)
  p)

(define test-read
  (case-lambda
    [(in) (main:read in #:source "input")]
    [(in expect)
     (define v (test-read in))
     (unless (equal? v expect)
       (error 'test "fail\n  got: ~s\n  expect: ~s"
              v
              expect))
     v]))

(test-read (s->p "#:a")
           '#:a)
(test-read (s->p "#\\a")
           #\a)
(test-read (s->p "#\\U3BB")
           #\u3BB)
(test-read (s->p "#\\\u3BB")
           #\u3BB)
(test-read (s->p "|ap ple|Pie")
           '|ap plePie|)
(test-read (s->p "(a b #%c)")
           '(a b #%c))
(test-read (s->p "(a #;z b . c)")
           '(a b . c))
(parameterize ([read-cdot #t])
  (test-read (s->p "(a b . c)")
             '(a (#%dot b c))))
(parameterize ([read-cdot #t])
  (test-read (s->p "a.b.c.d|.|f")
             '(#%dot (#%dot (#%dot a b) c) d.f)))
(test-read (s->p "(b . a #| a |# . c)")
           '(a b c))
(test-read (s->p "(a 1.0 ; comment\n c)")
           '(a 1.0 c))
(test-read (s->p "(a \"1.0\" c)")
           '(a "1.0" c))
(test-read (s->p "'('a `b ,c ,@d ,@ e #'f #`g #,h #,@i)")
           ''('a `b ,c ,@d ,@e #'f #`g #,h #,@i))
(test-read (s->p "(#t)")
           '(#t))
(test-read (s->p "(#TRUE)")
           '(#t))
(test-read (s->p "#ci (#fAlSe)")
           '(#f))
(test-read (s->p "#005(fAl Se)")
           '#(fAl Se Se Se Se))
(let ([ht (test-read (s->p "#1=#hasheq((#1# . #1#))"))])
  (unless (eq? (hash-ref ht ht) ht)
    (error 'test "fail for cyclic hash table")))
(test-read (s->p "#hash{(fAl . Se) (7 . 9)}")
           #hash{(fAl . Se) (7 . 9)})
(test-read (s->p "#s(fAl Se)")
           #s(fAl Se))
(test-read (s->p "#&fox")
           #&fox)
(test-read @s->p{#px#"fox"}
           #px#"fox")
(test-read (s->p "{fAl Se}")
           '(fAl Se))
(test-read (s->p "#! ok \\\n more\n 8")
           8)
(test-read @s->p{"apple\n\"\x30\7\07\u3BB\U1F600\uD83D\uDE00"}
           "apple\n\"\x30\7\07\u3BB\U1F600\U1F600")
(test-read @s->p{#"apple\n\"\x30\7\07"}
           #"apple\n\"0\a\a")
(test-read @s->p{#<<HERE
                This is text and
                HERE we go
                to finish the text
                HERE
                not included}
           "This is text and\nHERE we go\nto finish the text\n")
(parameterize ([read-curly-brace-with-tag #t])
  (test-read (s->p "{fAl Se}")
             '(#%braces fAl Se)))
(parameterize ([read-case-sensitive #f])
  (test-read (s->p "Case\\InSens")
             'caseInsens))
(with-handlers ([exn:fail:read? exn-message])
  (test-read (s->p "{  fAl\n Se)")))

(parameterize ([current-readtable (make-readtable #f
                                                  #\$ #\( #f
                                                  #\% #\) #f)])
  (test-read (s->p "$inside%")
             '(inside)))
(parameterize ([current-readtable (make-readtable #f
                                                  #\t 'terminating-macro
                                                  (lambda (a b c d e f) 'TEE)
                                                  #\u 'non-terminating-macro
                                                  (lambda (a b c d e f) 'YOO))])
  (test-read (s->p "(1t2u3)")
             '(1 TEE 2u3)))
(parameterize ([current-readtable (make-readtable #f
                                                  #\t 'dispatch-macro
                                                  (lambda (a b c d e f) 'TEE))])
  (test-read (s->p "(1 #t 2)")
             '(1 TEE 2)))
(parameterize ([read-accept-reader #t])
  (main:read (s->p "#readerok") #:dynamic-require (lambda (lib sym fail-k)
                                                    (lambda (in src line col pos)
                                                      'OK))))
(parameterize ([read-accept-reader #t])
  (main:read (s->p "#lang ok ?") #:dynamic-require (lambda (lib sym fail-k)
                                                     (lambda (in src line col pos)
                                                       'LANG-OK))))
(parameterize ([read-accept-reader #t])
  (main:read (s->p "#!ok ?") #:dynamic-require (lambda (lib sym fail-k)
                                                 (lambda (in src line col pos)
                                                   '|#!-OK|))))

(define s (format "~s" (for/list ([i 100000])
                         i)))
(void (time (test-read (s->p s))))
(void (time (read (s->p s))))

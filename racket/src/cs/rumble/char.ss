
(define/who (char-blank? x)
  (check who char? x)
  (or (char=? x #\tab)
      (eq? (char-general-category x) 'Zs)))

(define/who (char-iso-control? x)
  (check who char? x)
  (or (char<=? #\nul x #\x1F)
      (char<=? #\delete x #\x9F)))
  
(define/who (char-punctuation? x)
  (check who char? x)
  (chez:memq (char-general-category x) '(Pc Pd Ps Pe Pi Pf Po)))

(define/who (char-graphic? x)
  (check who char? x)
  (or (char-numeric? x)
      (char-alphabetic? x)
      (chez:memq (char-general-category x) '(Ll Lm Lo Lt Lu Nd Nl No Mn Mc Me
                                                ;; char-symbolic?:
                                                Sm Sc Sk So
                                                ;; char-punctuation?:
                                                Pc Pd Ps Pe Pi Pf Po))))

(define/who (char-symbolic? x)
  (check who char? x)
  (chez:memq (char-general-category x) '(Sm Sc Sk So)))

(define (interned-char? v)
  (and (char? v) (< (char->integer v) 256)))

;; FIXME
(define (make-known-char-range-list)
  '((0 255 #f)))

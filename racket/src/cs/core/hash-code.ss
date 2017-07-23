;;; Parts from "newhash.ss" in Chez Scheme's implementation

;;; newhash.ss
;;; Copyright 1984-2016 Cisco Systems, Inc.
;;; 
;;; Licensed under the Apache License, Version 2.0 (the "License");
;;; you may not use this file except in compliance with the License.
;;; You may obtain a copy of the License at
;;; 
;;; http://www.apache.org/licenses/LICENSE-2.0
;;; 
;;; Unless required by applicable law or agreed to in writing, software
;;; distributed under the License is distributed on an "AS IS" BASIS,
;;; WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
;;; See the License for the specific language governing permissions and
;;; limitations under the License.

(define codes (make-weak-eq-hashtable))
(define counter 12345)

(define (eq-hash-code x)
  (cond
   [(symbol? x) (symbol-fast-hash x)]
   [(number? x) (number-hash x)]
   [(char? x) (char->integer x)]
   [else
    (or (eq-hashtable-ref codes x #f)
        (let ([c (fx1+ counter)])
          (set! counter c)
          (eq-hashtable-set! codes x counter)
          c))]))

(define (symbol-fast-hash sym)
  ;; Avoid forcing the universal name of a gensym when hashing
  (if (gensym? sym)
      (or (getprop sym 'racket-gensym-hash-code)
          (let ([c (fx1+ counter)])
            (set! counter c)
            (putprop sym 'racket-gensym-hash-code c)
            c))
      (symbol-hash sym)))

(define number-hash
  (lambda (z)
    (cond
     [(fixnum? z) (if (fx< z 0) (fxnot z) z)]
     [(flonum? z) (#3%$flhash z)]
     [(bignum? z) (modulo z (most-positive-fixnum))]
     [(ratnum? z) (number-hash (+ (* (numerator z) 5) (denominator z)))]
     [else (logxor (lognot (number-hash (real-part z))) (number-hash (imag-part z)))])))

(define (eqv-hash-code x)
  (cond
   [(number? x) (number-hash x)]
   [(char? x) (char->integer x)]
   [else (eq-hash-code x)]))

(define (equal-hash-code x)
  (equal-hash x))

(define (hash-code-combine hc v)
  (bitwise-and (+ (bitwise-arithmetic-shift-left hc 2)
                  v)
               (greatest-fixnum)))

(define (hash-code-combine-unordered hc v)
  (bitwise-and (+ hc v)
               (greatest-fixnum)))

#lang racket/base
(require "bootstrap-thread-main.rkt")

;; Don't use exceptions here; see "../thread/demo.rkt"

(define done? #f)

(call-in-main-thread
 (lambda ()

   (printf "Enter to continue after confirming process sleeps...\n")
   (read-line)
   
   (set! done? #t)))

(unless done?
  (error "main thread stopped running due to deadlock?"))

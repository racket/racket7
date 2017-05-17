(define-struct date (second
                     minute
                     hour
                     day
                     month
                     year
                     week-day
                     year-day
                     dst?
                     time-zone-offset))

(define-struct date* date (nanosecond time-zone-name))

(define (time->ms t)
  (+ (* 1000. (time-second t))
     (/ (time-nanosecond t) 1000000.)))

(define (time-apply f extra)
  (let ([stats (statistics)])
    (call-with-values (lambda () (apply f extra))
      (lambda args
        (let ([new-stats (statistics)])
          (values
           args
           (inexact->exact (floor (time->ms
                                   (time-difference (sstats-cpu new-stats)
                                                    (sstats-cpu stats)))))
           (inexact->exact (floor (time->ms
                                   (time-difference (sstats-real new-stats)
                                                    (sstats-real stats)))))
           (inexact->exact (floor (time->ms
                                   (time-difference (sstats-gc-cpu new-stats)
                                                    (sstats-gc-cpu stats)))))))))))

(define (current-gc-milliseconds)
  (let ([stats (statistics)])
    (sstats-gc-cpu stats)))

(define (current-milliseconds)
  (inexact->exact (floor (current-inexact-milliseconds))))

(define (current-inexact-milliseconds)
  (time->ms (current-time 'time-utc)))

(define (current-seconds)
  (let ((t (current-time 'time-utc)))
    (time-second t)))

(define (seconds->date s)
  (define off (date-zone-offset (current-date)))
  (define tm (make-time 'time-utc (floor s) (/ (- s (floor s)) 1000000000)))
  (define d (time-utc->date tm off))
  (make-date (chez:date-second d)
             (chez:date-minute d)
             (chez:date-hour d)
             (chez:date-day d)
             (chez:date-month d)
             (chez:date-year d)
             (chez:date-week-day d)
             (chez:date-year-day d)
             #f ;(chez:date-dst? d) ;; FIXME when chez supports this
             (date-zone-offset d)))

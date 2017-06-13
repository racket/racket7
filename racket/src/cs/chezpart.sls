;; Reexports from `chezscheme` bindings that won't be replaced
;; by Racket-specific implementations.

(library (chezpart)
  (export)
  (import (chezscheme))
  (export (import
           (rename (except (chezscheme)
                           sort
                           force delay identifier?
                           output-port-buffer-mode
                           read-char peek-char char-ready?
                           make-input-port make-output-port
                           close-input-port close-output-port
                           list? input-port? output-port?
                           open-input-file open-output-file abort
                           current-output-port current-input-port current-directory
                           open-input-string open-output-string get-output-string
                           open-input-output-file
                           with-input-from-file with-output-to-file
                           call-with-input-file call-with-output-file
                           file-position
                           write display newline port-name port-closed? write-char
                           print-graph print-vector-length
                           date? make-date
                           dynamic-wind
                           call-with-current-continuation
                           make-engine engine-block engine-return
                           current-eval load
                           sleep thread? buffer-mode?
                           exit-handler
                           equal?
                           vector? mutable-vector? vector-length vector-ref vector-set!
                           vector-copy vector-fill! vector->immutable-vector
                           random random-seed
                           box? unbox set-box!)
                   [make-parameter chez:make-parameter]

                   [void chez:void]
                   [date-second chez:date-second]
                   [date-minute chez:date-minute]
                   [date-hour chez:date-hour]
                   [date-day chez:date-day]
                   [date-month chez:date-month]
                   [date-year chez:date-year]
                   [date-week-day chez:date-week-day]
                   [date-year-day chez:date-year-day]
                   [date-dst? chez:date-dst?]
                   [string-copy! chez:string-copy!]
                   [apply chez:apply]
                   [procedure? chez:procedure?]
                   [procedure-arity-mask chez:procedure-arith-mask]
                   [substring chez:substring]
                   [gensym chez:gensym]
                   [symbol->string chez:symbol->string]
                   [fprintf chez:fprintf]
                   [printf chez:printf]
                   [format chez:format]
                   [current-error-port chez:current-error-port]
                   [string->number chez:string->number]
                   [file-exists? chez:file-exists?]
                   [directory-list chez:directory-list]
                   [delete-file chez:delete-file]
                   [delete-directory chez:delete-directory]
                   [filter chez:filter]
                   [member chez:member]
                   [memv chez:memv]
                   [memq chez:memq]
                   [error chez:error]
                   [raise chez:raise]
                   [exit chez:exit]
                   [vector-sort! chez:vector-sort!]
                   [vector-sort chez:vector-sort]))))

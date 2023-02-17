; Read-print loop that uses read-datum for parsing input.
;
; Unlike repl.scm, this does not evaluate the datums that are read. It
; just prints them back out.
;
; Project UID 7e390a38edc65081bf76ab8edd67fe9d208befb9

(load "parser.scm")

; The read-print loop:
; 1) Uses call/cc to invoke error-setup, passing the REPL's
;    continuation to the latter, which stores it for error handling.
; 2) Prints an interpeter prompt.
; 3) (Read)s a datum from standard input.
; 5) (Print)s the datum.
; 6) Tail-recursively calls itself.
; The loop exists upon receiving an end-of-file.
(define (read-print-loop)
  (call-with-current-continuation error-setup)
  (display "read> ")
  (flush-output)  ; non-standard plt-r5rs procedure
  (let ((datum (read-datum)))            ; read
    (if (not (eof-object? datum))
        (begin (write datum)             ; print
               (newline)
               (read-print-loop)         ; move to next iteration
        )
        (newline)  ; exit, printing a newline on the way out
    )
  )
)

(read-print-loop)

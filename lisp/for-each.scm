(define one-to-four (list 1 2 3 4))
(define (for-each proc list)
  (cond ((null? list) "done")
        (else (proc (car list))
              (for-each proc
                        (cdr list)))))
(for-each (lambda (item) (display item) (display ", "))
          one-to-four)

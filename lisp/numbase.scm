; liczba zdefiniowana przez listę jej cyfr oraz podstawę układu liczbowego
(define (make-number digits base)
  (cons digits base))
(define (digits number)
  (car number))
(define (base number)
  (cdr number))

(define (pow a b)
  (cond ((= 1 b) a)
        ((= 0 b) 1)
        (else    (* a (pow a (- b 1))))))

(define (print-number number)
  (display (car (digits number))) (display '*)
  (display (base number)) (display '^)
  (display (- (length (digits number))
              1))
  (if (> (length (digits number)) 1)
      (begin (display " + ")
             (print-number (make-number (cdr (digits number))
                                        (base number))))))

(define (number-to-int number)
  (if (= (length (digits number)) 0)
      0
      (+ (* (car (digits number))
            (pow (base number)
                 (- (length (digits number)) 1)))
         (number-to-int (make-number (cdr (digits number))
                                     (base number))))))

(define (int-to-number x base)
  (define (int-to-digits x base)
    (if (= (quotient x base)
           (modulo x base)
           0)
        ()
        (append (int-to-digits (quotient x base) base)
                (list (modulo x base)))))
  (make-number (int-to-digits x base) base))


(define n 3) ; oryginalna podstawa układu
(define p 5) ; docelowa podstawa układu
(define a (make-number (list 1 2 0 2) n))
(define x (number-to-int a))
(define b (int-to-number x p))

(print-number a)
(display " = ") (display x) (newline)
(print-number b)
(display " = ") (display x) (newline)

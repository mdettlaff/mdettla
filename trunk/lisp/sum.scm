(define (sum init n term)
  (if (> init n)
      0
      (+ (term n) (sum init (- n 1) term))))

; potęga zdefiniowana iteracyjnie
(define (pow a b)
  (define (pow-iter a b prod i)
    (if (= i b)
        prod
        (pow-iter a b (* prod a) (1+ i))))
  (pow-iter a b 1 0))

; potęga zdefiniowana rekursywnie
(define (pow a b)
  (if (= 1 b)
      a
      (* a (pow a (- b 1)))))

(define init 1)
(define n 10)
(display "Suma i=") (display init) (display "..") (display n)
(display ", 1/(2^i)") (newline)
(display (sum init
              n
              (lambda (i) (/ 1 (pow 2 i)))))

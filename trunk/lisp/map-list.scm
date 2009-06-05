(define one-to-four (list 1 2 3 4))
(define (square x) (* x x))

(define (scale-list s l)
  (if (null? l)
      ()
      (cons (* (car l) s)
            (scale-list s (cdr l)))))

(define (map-list f l)
  (if (null? l)
      ()
      (cons (f (car l))
            (map-list f (cdr l)))))

(display one-to-four)
(display (scale-list 2 one-to-four))
(display (map-list square one-to-four))

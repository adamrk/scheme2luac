(define g (lambda (x y)
    (if (< x 0)
        y
        (g (- x 1) (+ y 1))
    )
))

(g 100000 0)

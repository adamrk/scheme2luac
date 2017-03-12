(define x 5)
(define (f) (lambda () (lambda () x)))
(((f)))

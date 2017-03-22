(define-syntax my-or
    (syntax-rules ()
        (
            (my-or)
            #f
        )
        (
            (my-or a)
            a
        )
        (
            (my-or a b c ...)
            (let ((x a)) (if x x (my-or b c ...)))
        )
    )
)

(my-or #f #f 2 3)

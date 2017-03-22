(define-syntax inc_macro
    (syntax-rules ()
        (
            (inc_macro a)
            (+ a 1)
        )
    )
)

(inc_macro (* 1 2 3))
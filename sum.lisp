(define sum-n  (lambda (n)
    (if (= n 0)
        0
        (+ n (sum-n (- n 1))
        )
    )
))

(sum-n 5)
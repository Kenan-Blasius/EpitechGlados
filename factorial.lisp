(define x 51435)

(define factorial
    (lambda (n)
        (if (= n 0)
            1
            (* n (factorial (- n 1))))))

(factorial x)

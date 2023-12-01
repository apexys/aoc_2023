#lang racket
(define infile (open-input-file "demo_input"))

(define (numbers-in-string str)
    ;Extract only the digits from the string
    (let ([number-string (sequence-fold 
        (lambda (acc c) (if (char-numeric? c) (string-append acc (string c)) acc))
        (string)
        str
    )])
        (+
        ;First digit is ten-valued
            (* (string->number (string (string-ref number-string 0))) 10)
        ;Last digit is one-valued
            (string->number (string(string-ref number-string (sub1 (string-length number-string)))))
        )
    )
)

(define (sum-of-file)
;Iterate all lines and sum
    (sequence-fold
        (lambda (acc line) (+ acc (numbers-in-string line)))
        0
        (in-lines infile)
    )
)

(writeln (sum-of-file))
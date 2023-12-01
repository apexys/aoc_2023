#lang racket
(define infile (open-input-file "input"))

(define replacement-list (list
    (list "one" 1)
    (list "two" 2)
    (list "three" 3)
    (list "four" 4)
    (list "five" 5)
    (list "six" 6)
    (list "seven" 7)
    (list "eight" 8)
    (list "nine" 9)
))


(define (extract-digits str)
    ;Iterate over both each character and each substring from that character onwards
    (foldl 
        (lambda (c i acc)
        ;If the character is already numeric, append it to the list
            (if (char-numeric? c)
                (append acc (list(string->number (string c))))
                ;Otherwise, check if we have a textual digit in the replacement list and push that
                (let ((string-from-here (substring str i)))
                    (let ((matching-digit
                            (filter 
                                (lambda (entry) (string-prefix? string-from-here (first entry)))
                                replacement-list
                            )
                        ))
                        (if (empty? matching-digit)
                            acc
                            (append acc (list(second(first matching-digit))))
                        )
                    
                    )
                )
            )
        )
        (list) ;start with empty list
        (sequence->list str) ;chars
        (sequence->list (in-range (string-length str))) ;char-indices
    )
)

;Add up the numbers
(define (numbers-in-list list)
        (+
        ;First digit is ten-valued
            (* (first list) 10)
        ;Last digit is one-valued
            (last list)
        )
)


(define (sum-of-file)
;Iterate all lines and sum
    (sequence-fold
        (lambda (acc line) (+ acc (numbers-in-list(extract-digits line))))
        0
        (in-lines infile)
    )
)

(writeln (sum-of-file))
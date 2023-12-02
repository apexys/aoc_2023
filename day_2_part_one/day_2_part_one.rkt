#lang racket
(define infile (open-input-file "input"))

; Extract an id and the information from a game line
(define (id-and-rest line) 
    (let (
        (matched  (regexp-match #px"Game (\\d+): (.+)" line))
    )
        (list (string->number (second matched)) (third matched))
    )
)

; Extract the actual games from a game line by splitting at ;
(define (games id-and-rest)
    (let (
        (rest (second id-and-rest))
    )
        (map
            (lambda (game) (string-trim game)) 
            (string-split rest ";")
        )
    )
)

;Extract a list of '(red green blue)
(define (game->rgb game)
    (let (
        (parts (string-split game ", "))
        )
        (foldl 
            (lambda (part acc) ;A part is something like "3 blue"
                (let 
                    ((part-split (string-split part " "))) ;Split the part at the space and bind to number and color
                    (let (
                            (number (string->number (first part-split)))
                            (color (second part-split))
                        )
                        ;Compute the update to the accumulator according to the color
                        (let ((update (match color
                            ("red" (list number 0 0))
                            ("green" (list 0 number 0))
                            ("blue" (list 0 0 number))
                        )))
                        ;apply the update
                            (map
                                (lambda (prev new) (+ prev new))
                                acc
                                update
                            )
                        )
                    )
                )
            )
            (list 0 0 0) ;Init: 0 red 0 green 0 blue
            parts
        )
    )
)

;The condition given by the task
(define condition (list 12 13 14))

;Check if a game is possible by checking each element against the condition
(define (game-possible? game)
    (andmap ;A game if possible if all entries in the r g b are less or equal to the condition
        (lambda (cond g)  (>= cond g))
        condition
        game
    )
)

; Return the id if all games in this set are possible or 0 if not
(define (id-if-all-possible id-and-rest)
    (if
        (andmap
                game-possible?
                (map 
                    game->rgb
                    (games id-and-rest)
                )
        )
        (first id-and-rest)
        0
    )
)

(display (sequence-fold
    (lambda (acc line) (+ (id-if-all-possible (id-and-rest line)) acc))
    0
    (in-lines infile)
))


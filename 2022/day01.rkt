#lang racket

(define (read-food-supplies)
  (for/fold ([elves '()]
             [current-elf '()]
             #:result (cons current-elf elves))
            ([line (in-lines)])
    (if (string=? line "")
        (values (cons current-elf elves) '())
        (values elves (cons (string->number line) current-elf)))))

(define (sum list) (foldl + 0 list))

(define food-supplies (read-food-supplies))
(define food-totals (sort (map sum food-supplies) >))
(printf "Part 1: ~S~%" (first food-totals))
(printf "Part 2: ~S~%" (sum (take food-totals 3)))

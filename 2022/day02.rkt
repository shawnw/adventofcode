#lang racket/base

(require racket/list racket/string)

(define (read-input)
  (for/list ([line (in-lines)])
    (map string->symbol (string-split line))))

(define play-table
  '#(#(tie win loss)
     #(loss tie win)
     #(win loss tie)))
(define (play elf you)
  (vector-ref (vector-ref play-table (index-of '(A B C) elf)) (index-of '(X Y Z) you)))

(define (score you outcome)
  (+ (case you
       ((X) 1)
       ((Y) 2)
       ((Z) 3))
     (case outcome
       ((win) 6)
       ((tie) 3)
       ((loss) 0))))

(define (xyz->outcome sym)
  (case sym
    ((Z) 'win)
    ((Y) 'tie)
    ((X) 'loss)))

(define desired-play-table
  '#(#(Y X Z)
     #(Z Y X)
     #(X Z Y)))
(define (what-to-play elf outcome)
  (vector-ref (vector-ref desired-play-table (index-of '(A B C) elf)) (index-of '(win tie loss) outcome)))

(define input (read-input))
(define part1 (for/sum ([round (in-list input)])
                (let* ([elf (first round)]
                       [you (second round)]
                       [outcome (play elf you)])
                           (score you outcome))))

(define part2 (for/sum ([round (in-list input)])
                (let* ([elf (first round)]
                       [outcome (xyz->outcome (second round))]
                       [you (what-to-play elf outcome)])
                  (score you outcome))))

(printf "Part 1: ~S~%" part1)
(printf "Part 2: ~S~%" part2)

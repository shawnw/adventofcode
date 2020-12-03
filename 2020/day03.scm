#!/usr/local/bin/csi -s
(import
 (srfi 1)
 (srfi 25)
 (chicken format)
 (chicken io))

(define (list->array lol)
  (let ((rows (length lol))
        (cols (length (car lol))))
    (apply array (shape 0 rows 0 cols) (concatenate lol))))

(define (solve1 forest right down)
  (let ((rows (array-end forest 0))
        (cols (array-end forest 1)))
    (let loop ((trees 0)
               (row 0)
               (col 0))
      (if (>= row rows)
          trees
          (loop (+ trees (if (char=? (array-ref forest row (remainder col cols)) #\#)
                             1 0))
                (+ row down)
                (+ col right))))))

(define (map-product f lst)
  (fold (lambda (elem acc) (* acc (f elem))) 1 lst))

(define (solve2 forest)
  (map-product (lambda (slope) (solve1 forest (car slope) (cdr slope)))
               '((1 . 1) (3 . 1) (5 . 1) (7 . 1) (1 . 2))))

(define input (list->array (map string->list (read-lines))))
(printf "Part 1: ~A~%" (solve1 input 3 1))
(printf "Part 2: ~A~%" (solve2 input))

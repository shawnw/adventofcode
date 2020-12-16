#!/usr/local/bin/csi -s
(import (chicken format)
        (chicken io)
        (chicken sort)
        (chicken string)
        (srfi 1)
        )
(declare (fixnum-arithmetic) (block))

(define (read-input #!optional (port (current-input-port)))
  (let* ((raw (read-lines port))
         (timestamp (string->number (car raw)))
         (busses (map (lambda (id) (if (string=? id "x") #f (string->number id)))
                      (string-split (cadr raw) ","))))
    (values timestamp busses)))

(define (next-arrival-from timestamp id)
  (+ (- timestamp (remainder timestamp id)) id))

(define (arrives-before a b) (< (cdr a) (cdr b)))

(define (solve1 timestamp busses)
  (let* ((busses (filter-map
                  (lambda (id)
                    (and id
                         (cons id (next-arrival-from timestamp id))))
                  busses))
         (sorted (sort! busses arrives-before))
         (next-bus (car sorted)))
    (* (car next-bus) (- (cdr next-bus) timestamp))))

;;; Use the chinese remainder theorem for part 2

(define (bezout_ids a b)
  (let loop ((old_r a) (r b)
             (old_s 1) (s 0)
             (old_t 0) (t 1))
    (if (= r 0)
        (values old_s old_t)
        (let ((q (quotient old_r r)))
          (loop r (- old_r (* q r))
                s (- old_s (* q s))
                t (- old_t (* q t)))))))

(define (existence a1 n1 a2 n2)
  (let-values (((m1 m2) (bezout_ids n1 n2)))
    (+ (* a1 m2 n2) (* a2 m1 n1))))

(define (congruence bus1 bus2)
  (let ((prod (* (car bus1) (car bus2)))
        (e (existence (cdr bus1) (car bus1) (cdr bus2) (car bus2))))
    (cons prod (modulo (+ e prod) prod))))

(define (solve2 busses)
  (let ((offsets (filter-map
                  (lambda (id i) (if id (cons id (if (= i 0) 0 (- id i))) #f))
                  busses
                  (iota (length busses)))))
    (cdr (reduce congruence #f offsets))))

(let-values (((timestamp busses) (read-input)))
  (printf "Part 1: ~A~%" (solve1 timestamp busses))
  (printf "Part 2: ~A~%" (solve2 busses)))

#!/usr/local/bin/csi -s
(import
 (chicken fixnum)
 (chicken format)
 (chicken io)
 (chicken sort)
 (srfi 1))

;;; Only used when compiling; use explicit fixnum operations for csi
;;; They're really just needed for division, but use everywhere
;;; anyways.
(declare (fixnum-arithmetic) (block))

(define-constant rows 128)
(define-constant columns 8)

(define (calculate-row r)
  (let loop ((low-row 0)
             (high-row (sub1 rows))
             (seats (string->list r)))
    (let ((midpoint (add1 (fx/ (fx- high-row low-row) 2))))
      (cond ((null? seats) low-row) ;; same as high-row by now
            ((char=? (car seats) #\F)
             (loop low-row (fx- high-row midpoint) (cdr seats)))
            ((char=? (car seats) #\B)
             (loop (fx+ low-row midpoint) high-row (cdr seats)))
            (else (error "Invalid character: " (car seats)))))))

(define (calculate-column c)
  (let loop ((low-col 0)
             (high-col (sub1 columns))
             (seats (string->list c)))
    (let ((midpoint (add1 (fx/ (fx- high-col low-col) 2))))
      (cond ((null? seats) low-col) ;; same as high-col by now
            ((char=? (car seats) #\L)
             (loop low-col (fx- high-col midpoint) (cdr seats)))
            ((char=? (car seats) #\R)
             (loop (fx+ low-col midpoint) high-col (cdr seats)))
            (else (error "Invalid character: " (car seats)))))))

(define (seat-id pass)
  (let ((row (calculate-row (substring pass 0 7)))
        (column (calculate-column (substring pass 7 10))))
    (fx+ (fx* row 8) column)))

(define (solve2 numbers)
  (let loop ((num1 numbers)
             (num2 (cdr numbers)))
    (cond ((null? num2) #f)
          ((fx= (add1 (car num1)) (car num2))
           (loop (cdr num1) (cdr num2)))
          (else
           (add1 (car num1))))))

(define passes (read-lines))
(define seat-ids (sort! (map seat-id passes) fx<))

(printf "Part 1: ~A~%" (last seat-ids))
(printf "Part 2: ~A~%" (solve2 seat-ids))

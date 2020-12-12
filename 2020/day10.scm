#!/usr/local/bin/csi -s
(import (chicken format)
        (chicken io)
        (chicken sort)
        (srfi 1))
(declare (fixnum-arithmetic) (block))

(define (vector-update! v i f)
  (vector-set! v i (f (vector-ref v i)))
  v)

(define (count-differences adapters)
  (let loop ((adapters adapters)
             (previous 0)
             (counts (vector 0 0 0 1)))
    (if (null? adapters)
        counts
        (let* ((adapter (car adapters))
               (diff (- adapter previous)))
          (if (> diff 3)
              (error "Too large a different between adapter " previous " and " adapter)
              (loop (cdr adapters)
                    adapter
                    (vector-update! counts diff add1)))))))

(define (solve1 adapters)
  (let ((diffs (count-differences adapters)))
    (* (vector-ref diffs 1) (vector-ref diffs 3))))

;;; Won't work well with large lists
(define (count-arrangements adapters prev)
  (cond
   ((null? adapters) 1) ; No more
   ((null? (cdr adapters)) 1) ; One more
   ((null? (cddr adapters)) ; Two more
    (if (> (- (cadr adapters) prev) 3) 1 2))
   (else ; Three or more left
    (let ((gt3 (> (- (caddr adapters) prev) 3))
          (gt2 (> (- (cadr adapters) prev) 3)))
      (cond
       ((and gt3 gt2)
        (count-arrangements (cdr adapters) (car adapters)))
       (gt3
        (+ (count-arrangements (cdr adapters) (car adapters))
           (count-arrangements (cddr adapters) (cadr adapters))))
       (else
        (+ (count-arrangements (cdr adapters) (car adapters))
           (count-arrangements (cddr adapters) (cadr adapters))
           (count-arrangements (cdddr adapters) (caddr adapters)))))))))

(define (partition-adapters adapters)
  (let* ((adapters (cons 0 adapters))
         (device (+ 3 (last adapters)))
         (adapters (append! adapters (list device))))
    (let loop ((adapters adapters)
               (acc1 '())
               (acc2 '()))
      (if (null? (cdr adapters))
          (reverse! (cons (reverse! (cons (car adapters) acc1)) acc2))
          (let ((current (car adapters)))
            (if (= (- (cadr adapters) current) 3)
                (loop (cdr adapters) '() (cons (reverse! (cons current acc1)) acc2))
                (loop (cdr adapters) (cons current acc1) acc2)))))))

(define (solve2 adapters)
  (let ((partitions (partition-adapters adapters)))
    (fold (lambda (part acc) (* acc (count-arrangements (cdr part) (car part))))
          1 partitions)))

(define input (sort! (read-list) <))
(printf "Part 1: ~A~%" (solve1 input))
(printf "Part 2: ~S~%" (solve2 input))

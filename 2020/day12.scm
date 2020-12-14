#!/usr/local/bin/csi -s
(import (chicken format)
        (chicken io)
        (srfi 13))
(declare (block))

(define (char->direction d)
  (case d
    ((#\N) 'north)
    ((#\E) 'east)
    ((#\S) 'south)
    ((#\W) 'west)))

(define (move direction distance)
  (case direction
    ((north) (make-rectangular 0 distance))
    ((east) (make-rectangular distance 0))
    ((south) (make-rectangular 0 (- distance)))
    ((west) (make-rectangular (- distance) 0))))

(define (rotate facing way degrees)
  (let ((degrees (if (char=? way #\L)
                     (case degrees ((90) 270) ((270) 90) ((180) 180))
                     degrees)))
    (case facing
      ((north)
       (case degrees
         ((90) 'east)
         ((180) 'south)
         ((270) 'west)))
      ((east)
       (case degrees
         ((90) 'south)
         ((180) 'west)
         ((270) 'north)))
      ((south)
       (case degrees
         ((90) 'west)
         ((180) 'north)
         ((270) 'east)))
      ((west)
       (case degrees
         ((90) 'north)
         ((180) 'east)
         ((270) 'south))))))

(define (pivot pos point way degrees)
  (let* ((degrees (if (char=? way #\L)
                      (case degrees ((90) 270) ((270) 90) ((180) 180))
                      degrees))
         (diff (- point pos))
         (x (real-part diff))
         (y (imag-part diff)))
  (case degrees
    ((180) (- pos diff))
    ((90) (+ pos (make-rectangular y (- x))))
    ((270) (- pos (make-rectangular y (- x)))))))

(define (distance p1 p2)
  (+ (abs (- (real-part p1) (real-part p2)))
     (abs (- (imag-part p1) (imag-part p2)))))

(define (split-string direction)
  (values (string-ref direction 0) (string->number (string-drop direction 1))))

(define (solve1 directions)
  (let loop ((facing 'east)
             (pos 0+0i)
             (directions directions))
    (if (null? directions)
        (distance pos 0+0i)
        (let-values (((command number) (split-string (car directions))))
          (case command
            ((#\N #\E #\W #\S)
             (loop facing (+ pos (move (char->direction command) number))
                   (cdr directions)))
            ((#\R #\L)
             (loop (rotate facing command number) pos (cdr directions)))
            ((#\F)
             (loop facing (+ pos (move facing number)) (cdr directions))))))))

(define (repadd num count)
  (let loop ((total 0)
             (count count))
    (if (= count 0)
        total
        (loop (+ total num) (- count 1)))))

(define (solve2 directions)
  (let loop ((pos 0+0i)
             (waypoint 10+1i)
             (directions directions))
    (if (null? directions)
        (distance pos 0+0i)
        (let-values (((command number) (split-string (car directions))))
          (case command
            ((#\N #\E #\W #\S)
             (loop pos (+ waypoint (move (char->direction command) number))
                   (cdr directions)))
            ((#\R #\L)
             (loop pos (pivot pos waypoint command number) (cdr directions)))
            ((#\F)
             (let* ((diff (- waypoint pos))
                    (offset (repadd diff number)))
               (loop (+ pos offset) (+ waypoint offset) (cdr directions)))))))))

(define input (read-lines))
(printf "Part 1: ~A~%" (solve1 input))
(printf "Part 2: ~A~%" (solve2 input))

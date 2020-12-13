#!/usr/local/bin/csi -s
(import (chicken format)
        (chicken io)
        (srfi 1)
        (srfi 13)
        (srfi 133))
(declare (fixnum-arithmetic) (block))

(define (room-ref room x y)
  (string-ref (vector-ref room y) x))
(define (room-set! room x y type)
  (string-set! (vector-ref room y) x type))
(define (copy-room old)
  (vector-map string-copy old))
(define (room=? rooma roomb) (vector= string=? rooma roomb))

(define (map-neighbors room x y f)
  (let* ((neighbors '())
         (max-x (string-length (vector-ref room 0)))
         (max-y (vector-length room))
         (diffs '((-1 . -1) (0 . -1) (1 . -1)
                  (-1 .  0)          (1 .  0)
                  (-1 .  1) (0 .  1) (1 .  1))))
    (filter-map (lambda (coord)
                  (and-let* ((newx (+ x (car coord)))
                             (newy (+ y (cdr coord)))
                             ((and (>= newx 0) (>= newy 0)
                                   (< newx max-x) (< newy max-y))))
                            (f newx newy coord)))
                diffs)))

(define (neighboring-tiles room x y)
  (map-neighbors room x y (lambda (newx newy _) (room-ref room newx newy))))

(define (visible-seats room x y)
  (let* ((max-x (string-length (vector-ref room 0)))
         (max-y (vector-length room))
         (in-range?
          (lambda (newx newy) (and (>= newx 0) (>= newy 0)
                                   (< newx max-x) (< newy max-y))))
         (find-visible
          (lambda (newx newy vec)
            (let loop ((newx newx)
                       (newy newy))
              (if (in-range? newx newy)
                  (case (room-ref room newx newy)
                    ((#\L #\#) => identity)
                    ((#\.) (loop (+ newx (car vec)) (+ newy (cdr vec)))))
                  #f)))))
    (map-neighbors room x y find-visible)))

(define (occupied-neighbors room x y)
  (count (cut char=? <> #\#) (neighboring-tiles room x y)))
(define (visible-occupied-seats room x y)
  (count (cut char=? <> #\#) (visible-seats room x y)))
(define (count-occupied room)
  (vector-fold (lambda (total row) (+ total (string-count row #\#))) 0 room))

(define (ticker tick-fun oldroom)
  (let ((newroom (copy-room oldroom))
        (max-x (string-length (vector-ref oldroom 0)))
        (max-y (vector-length oldroom)))
    (let loopy ((y 0))
      (if (= y max-y)
          newroom
          (let loopx ((x 0))
            (if (= x max-x)
                (loopy (+ y 1))
                (begin
                  (tick-fun newroom oldroom x y)
                  (loopx (+ x 1)))))))))

(define (rules-part1 newroom oldroom x y)
  (cond
   ((and
     (char=? (room-ref oldroom x y) #\L)
     (= 0 (occupied-neighbors oldroom x y)))
    (room-set! newroom x y #\#))
   ((and
     (char=? (room-ref oldroom x y) #\#)
     (>= (occupied-neighbors oldroom x y) 4))
    (room-set! newroom x y #\L))))

(define (rules-part2 newroom oldroom x y)
  (cond
   ((and
     (char=? (room-ref oldroom x y) #\L)
     (= 0 (visible-occupied-seats oldroom x y)))
    (room-set! newroom x y #\#))
   ((and
     (char=? (room-ref oldroom x y) #\#)
     (>= (visible-occupied-seats oldroom x y) 5))
    (room-set! newroom x y #\L))))

(define (solver room tick-fun)
  (let loop ((room (ticker tick-fun room))
             (oldroom room))
    (if (room=? room oldroom)
        (count-occupied room)
        (loop (ticker tick-fun room) room))))
(define (solve1 room) (solver room rules-part1))
(define (solve2 room) (solver room rules-part2))

(define input (list->vector (read-lines)))
(printf "~A~%" (solve1 input))
(printf "~A~%" (solve2 input))

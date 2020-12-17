#!/usr/local/bin/csi -s
(import (chicken format)
        (chicken io)
        (chicken irregex)
        (chicken sort)
        (chicken string)
        (srfi 1)
        (srfi 13)
        (srfi 133))
(declare (fixnum-arithmetic) (block))

(define range-re
  (sre->irregex '(: (=> name (+ (or alphabetic space))) #\: space
                    (=> range1min (+ digit)) #\- (=> range1max (+ digit))
                    " or "
                    (=> range2min (+ digit)) #\- (=> range2max (+ digit)))))

(define (parse-input #!optional (port (current-input-port)))
  (let* ((fields (let loop ((line (read-line port))
                            (fields '()))
                   (if (string=? line "")
                       fields
                       (and-let*
                        ((match (irregex-match range-re line)))
                        (loop (read-line port)
                              (cons
                               (cons
                                (irregex-match-substring match 'name)
                                (vector
                                 (string->number
                                  (irregex-match-substring match 'range1min))
                                 (string->number
                                  (irregex-match-substring match 'range1max))
                                 (string->number
                                  (irregex-match-substring match 'range2min))
                                 (string->number
                                  (irregex-match-substring match 'range2max))))
                               fields))))))
        (ticket (let ((line1 (read-line port)))
                  (map string->number (string-split (read-line port) ","))))
        (nearby-tickets (let* ((blank-line (read-line port))
                               (line1 (read-line port)))
                          (map
                           (lambda (line)
                             (map string->number (string-split line ",")))
                           (read-lines port)))))
    (values (list->vector fields) ticket nearby-tickets)))

(define (num-in-range? num field)
  (let ((vals (cdr field)))
    (or (and (<= (vector-ref vals 0) num) (<= num (vector-ref vals 1)))
        (and (<= (vector-ref vals 2) num) (<= num (vector-ref vals 3))))))

(define (in-range? num fields)
  (vector-any (cut num-in-range? num <>) fields))

(define (solve1 fields nearby-tickets)
  (fold (lambda (ticket total)
          (fold (lambda (num total) (if (in-range? num fields) total (+ total num)))
                total ticket))
        0 nearby-tickets))

(define (fields-valid-at-position tickets fields n)
  (filter (lambda (field)
            (every (lambda (ticket) (num-in-range? (vector-ref ticket n) field)) tickets))
          (vector->list fields)))

(define (field-remove-except! fields n name)
  (do ((i 0 (+ i 1)))
      ((= i (vector-length fields)))
    (if (not (= n i))
        (vector-set! fields i (alist-delete name (vector-ref fields i) string=?)))))

(define (solution? fields)
  (vector-every (lambda (field) (= (length field) 1)) fields))

(define (find-permutation fields)
  (let ((ordered-fields
         (sort (vector-map cons fields (list->vector (iota (vector-length fields))))
               (lambda (a b) (< (length (car a)) (length (car b)))))))
    (let loop ((n 0)
             (v2 fields))
    (if (= n (vector-length v2))
        (values (solution? v2) v2)
        (let loop2 ((possible-fields (vector-ref v2 (cdr (vector-ref ordered-fields n)))))
          (if (null? possible-fields)
              (values #f #f)
              (let ((v3 (vector-copy v2))
                    (pos (cdr (vector-ref ordered-fields n))))
                (vector-set! v3 pos (list (car possible-fields)))
                (field-remove-except! v3 pos (caar possible-fields))
                (let-values (((succ order) (loop (+ n 1) v3)))
                  (if succ
                      (values succ order)
                      (loop2 (cdr possible-fields)))))))))))

(define (solve2 fields my-ticket nearby-tickets)
  (let ((valid-tickets
         (map list->vector
              (cons my-ticket
                    (filter (lambda (ticket) (every (cut in-range? <> fields) ticket))
                            nearby-tickets))))
        (valid-fields (make-vector (vector-length fields) #f)))
    ;;; Populate a vector with only the fields that can be used in that position.
    (do ((n 0 (+ n 1)))
        ((= n (vector-length valid-fields)))
      (vector-set! valid-fields n (fields-valid-at-position valid-tickets fields n)))
    ;;; And try to find an ordering
    (let-values (((succ order) (find-permutation valid-fields)))
      (if succ
          (vector-fold (lambda (prod field num)
                         (if (string-prefix? "departure " (caar field))
                             (* prod num)
                             prod))
                       1 order (list->vector my-ticket))
          #f))))

(let-values (((fields ticket nearby-tickets) (parse-input)))
  (printf "Part 1: ~A~%" (solve1 fields nearby-tickets))
  (printf "Part 2: ~A~%" (solve2 fields ticket nearby-tickets)))

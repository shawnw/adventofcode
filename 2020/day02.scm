#!/usr/local/bin/csi -s
(import (chicken format)
        (chicken io)
        (chicken irregex)
        (srfi 1)
        (srfi 13))
(declare (fixnum-arithmetic) (block))

;;; Return all submatches as values
(define (irregex-matched-substrings match #!optional include-full-match)
  (let ((end-idx (if include-full-match -1 0)))
    (let loop ((idx (irregex-match-num-submatches match)) (acc '()))
      (if (= idx end-idx)
          (apply values acc)
          (loop (sub1 idx) (cons (irregex-match-substring match idx) acc))))))

(define (only-one . args) (= (count identity args) 1))

(define (solve-part1 input)
  (let* ((re (irregex "^(\\d+)-(\\d+) (.): (.*)"))
         (matches (lambda (line)
                    (and-let* ((match (irregex-match re line)))
                              (let-values (((min-count max-count char password)
                                            (irregex-matched-substrings match)))
                                (let ((min-count (string->number min-count))
                                      (max-count (string->number max-count))
                                      (count
                                       (string-count password (string-ref char 0))))
                                  (and (>= count min-count) (<= count max-count))))))))
    (count matches input)))

(define (solve-part2 input)
  (let* ((re (irregex "^(\\d+)-(\\d+) (.): (.*)"))
         (matches (lambda (line)
                    (and-let* ((match (irregex-match re line)))
                              (let-values (((idx1 idx2 char password)
                                            (irregex-matched-substrings match)))
                                (let ((idx1 (sub1 (string->number idx1)))
                                      (idx2 (sub1 (string->number idx2)))
                                      (char (string-ref char 0)))
                                  (only-one
                                   (char=? (string-ref password idx1) char)
                                   (char=? (string-ref password idx2) char))))))))
    (count matches input)))

(define input (read-lines))
(printf "Part 1: ~A~%" (solve-part1 input))
(printf "Part 2: ~A~%" (solve-part2 input))

#!/usr/local/bin/csi -s
(import (chicken format)
        (chicken io)
        (chicken irregex)
        (srfi 1)
        (srfi 13))

(define record-name-re (sre->irregex '(: bow (+ alpha) (look-ahead #\:))))
;;; For some reason a look-behind for the colon only matches once.
(define record-value-re (sre->irregex '(: #\: (+ (~ whitespace)))))
(define (record-fields raw-record)
  (let ((names (map string->symbol (irregex-extract record-name-re raw-record)))
        (vals (map (cut string-drop <> 1) (irregex-extract record-value-re raw-record))))
    (map cons names vals)))

(define (read-record #!optional (port (current-input-port)))
  (let loop ((lines '()))
    (let ((line (read-line port)))
      (if (or (eof-object? line) (string=? line ""))
          (record-fields (string-join lines))
          (loop (cons line lines))))))

(define (read-records #!optional (port (current-input-port)))
  (let loop ((records '()))
    (let ((record (read-record port)))
      (if (null? record)
          records
          (loop (cons record records))))))

(define height-re (sre->irregex '(or (: ($ (+ digit)) "cm")
                                     (: ($ (+ digit)) "in"))))
(define hair-re (sre->irregex '(: #\# (= 6 hex-digit))))
(define pid-re (sre->irregex '(= 9 digit)))

(define-syntax bounded?
  (syntax-rules ()
    ((_ str min max) (and-let* ((num (string->number str)))
                               (and (>= num min) (<= num max))))))

(define mandatory-fields
  `((byr . ,(lambda (v) (bounded? v 1920 2002)))
    (iyr . ,(lambda (v) (bounded? v 2010 2020)))
    (eyr . ,(lambda (v) (bounded? v 2020 2030)))
    (hgt . ,(lambda (v)
              (and-let* ((match (irregex-match height-re v)))
                        (let ((metric (irregex-match-substring match 1))
                              (imperial (irregex-match-substring match 2)))
                          (if metric
                              (bounded? metric 150 193)
                              (bounded? imperial 59 76))))))
    (hcl . ,(lambda (v) (irregex-match? hair-re v)))
    (ecl . ,(lambda (v) (member v '("amb" "blu" "brn" "gry" "grn" "hzl" "oth") string=?)))
    (pid . ,(lambda (v) (irregex-match? pid-re v)))))

(define (solve records)
  (let ((part1-valid-record?
         (lambda (record)
           (every (lambda (validator) (assq (car validator) record)) mandatory-fields)))
        (part2-valid-record?
         (lambda (record)
           (every (lambda (validator)
                    (and-let* ((val (assq (car validator) record)))
                              ((cdr validator) (cdr val))))
                  mandatory-fields))))
    (let loop ((part1 0)
               (part2 0)
               (records records))
      (if (null? records)
          (values part1 part2)
          (loop (+ part1 (if (part1-valid-record? (car records)) 1 0))
                (+ part2 (if (part2-valid-record? (car records)) 1 0))
                (cdr records))))))

(let-values (((part1 part2) (solve (read-records))))
  (printf "Part 1: ~A~%" part1)
  (printf "Part 2: ~A~%" part2))

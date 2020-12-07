#!/usr/local/bin/csi -s
(import (chicken format)
        (chicken io)
        (chicken irregex)
        (chicken string)
        (srfi 1))

(define bag-re
  (sre->irregex
   '(: (=> color (+ nonl)) " bags contain "
       (=> contains (or "no other bags."
              (+ (+ digit) (+ space) (+ any) " bag" (? #\s) (or #\.  #\,)))))))
(define one-bag-re
  (sre->irregex '(: bol (* space) (=> number (+ digit)) (+ space)
                    (=> color (+ nonl)) " bag")))

(define (color->symbol color)
  (string->symbol (string-translate color #\space #\-)))

(define (parse-rule line)
  (let* ((bag-match (irregex-match bag-re line))
         (color (color->symbol (irregex-match-substring bag-match 'color)))
         (contains (string-split (irregex-match-substring bag-match 'contains) ",")))
    (cons color
          (let ((contents
                 (map (lambda (sub-bag)
                        (and-let*
                         ((sub-bag-match (irregex-search one-bag-re sub-bag)))
                         (cons
                          (color->symbol (irregex-match-substring sub-bag-match 'color))
                          (string->number
                           (irregex-match-substring sub-bag-match 'number)))))
                      contains)))
            (if (car contents)
                contents
                #f)))))

(define (can-contain? bag color)
  (and (cdr bag) (assq color (cdr bag))))
(define (bags-that-can-contain bags bag-color)
  (map car (filter (cut can-contain? <> bag-color) bags)))
(define (make-contains-alist bags)
  (map (lambda (bag) (cons (car bag) (bags-that-can-contain bags (car bag)))) bags))

(define (bag-roots bags color)
  (letrec ((contains-alist (make-contains-alist bags))
           (helper (lambda (color)
                     (let ((contains (assq color contains-alist)))
                       (append (cdr contains) (append-map helper (cdr contains)))))))
    (delete-duplicates! (helper color) eq?)))

(define (contains-count bags color)
  (let ((contents (assq color bags)))
    (if (cdr contents)
        (fold (lambda (holds acc)
                (+ acc (* (cdr holds) (contains-count bags (car holds)))))
                1 (cdr contents))
        1)))

;;; How many bags can ultimately contain a bag of color?
(define (solve1 bags color)
  (length (bag-roots bags color)))

;;; How many bags must 1 bag of color contain?
(define (solve2 bags color) (sub1 (contains-count bags color)))

(define input (map parse-rule (read-lines)))
(define my-bag 'shiny-gold)

(printf "Part 1: ~A~%" (solve1 input my-bag))
(printf "Part 2: ~A~%" (solve2 input my-bag))

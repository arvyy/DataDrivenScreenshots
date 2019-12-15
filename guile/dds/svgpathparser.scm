#!

(define (skip-while lst pred)
  (define-values (skipped rest) (take-while lst pred))
  rest)

(define (take-while lst pred)
  (define (take-while* lst acc)
    (match lst
      (((and el (? pred)) . rest) (take-while* (cdr lst) (cons (car lst) acc)))
      (_ (values (reverse acc) lst))))
  (take-while* lst '()))

(define (parse-letter lst)
  (cond
    ((null? lst) (values #f lst))
    (else (let ((c (car lst)))
            (cond
              ((char-alphabetic? c) (values c lst))
              (else (error (format #f "Expected letter, got ~a" c))))))))

(define (parse-int-str lst)
  (define-values (int-str rest)(take-while lst char-numeric?)))

(define (parse-number lst)
  (define-values (whole-part rest-lst) (parse-int-str lst))
  (match rest-lst
     ((#\. . rest)
      (let-values (((frac-part rest-lst) (parse-int-str rest)))
        (values (string->number (string-append whole-part "." frac-part)) rest-lst)))
     (_ (values (string->number whole-part) rest-lst))))

(define (parse-tokens lst)
  (define (parse-tokens* tokens lst)
    (define lst/trim (skip-while lst (lambda(c)(or (char-whitespace? c) (equal? c #\,)))))
    (cond
      ((null? lst/trim) (reverse tokens))
      ((char-numeric? (car lst/trim))
       (let-values (((number rest-lst) (parse-number lst/trim)))
         (parse-tokens* (cons number tokens) rest-lst)))
      (else
        (let-values (((cmd rest-lst) (parse-letter lst/trim)))
          (parse-tokens* (cons cmd tokens) rest-lst)))))
  (parse-tokens* '() lst))
!#

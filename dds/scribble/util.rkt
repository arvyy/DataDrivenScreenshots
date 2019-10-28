#lang racket

(require scribble/base)

(define (render-fn fn-name req-list opt-list key-list returns . description)
  (list 
    (section fn-name #:tag fn-name)
    description
    (linebreak)
    (linebreak)
    "(" (bold fn-name) 
    (for/list ([req req-list])
      (list " " (car req)))
    (if (not (null? opt-list)) " [ " "")
    (for/list ([opt opt-list])
      (list " " (car opt)))
    (if (not (null? opt-list)) " ] " "")
    (for/list ([key key-list])
      (list (linebreak) (hspace 8) "#:" (italic (car key)) " " (car key)))
    ")"
    (linebreak)

    (apply itemlist (append (reqs->items req-list)
                            (opts->items opt-list)
                            (keys->items key-list)))

    (linebreak) returns))

(define (reqs->items req-list)
  (map (lambda(r)
         (item (car r) " - " (cdr r))) 
       req-list))

(define (opts->items opt-list)
  (map (lambda(o)
         (item (italic "optional") " " (car o) " - " (cadr o) ".( Default: " (caddr o) ")"))
       opt-list))

(define (keys->items key-list)
  (map (lambda(key)
         (item (italic "#:" (car key)) " " (car key) " - " (cadr key) ". (Default: " (caddr key) ")"))
       key-list))

(define id `("id" ("id; used in " ,(secref "override")) "#f"))
(define fill `("fill" (,(secref "color") " to fill inside the shape, or #f to disable") "(color 200 100 100)"))
(define stroke `(("stroke" (,(secref "color") " to stroke shape, or #f to disable") "(color 10 10 10)")
                 ("stroke-width" "thickness of the stroke" "1")))
(define fill+stroke `(,fill ,@stroke))


(provide (all-defined-out))

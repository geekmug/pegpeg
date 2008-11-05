(load "peg.ss")

(import (peg))

(case-sensitive #t)

(define expr-parser
 '(peg-parser
    [(e Expr) (p Product) (v Value) (n Number) (d Digit)]
    (Expr
      [(p1 "+" p2) (+ p1 p2)]
      [(p1 "-" p2) (- p1 p2)]
      [p p])
    (Product
      [(v1 "*" v2) (* v1 v2)]
      [(v1 "/" v2) (/ v1 v2)]
      [v v])
    (Value
      [("(" e ")") e]
      [n n])
    (Number
      [(+ d2)
       (let loop ([n 0] [d2 d2])
         (if (null? d2)
           n
           (loop (+ (* 10 n) (car d2)) (cdr d2))))])
    (Digit
      ["0" 0] ["1" 1] [,#\2 2] ["3" 3] ["4" 4]
      ["5" 5] ["6" 6] ["7" 7] ["8" 8] ["9" 9])))

; L = {a^n b^n c^n : n>= 1}
(define abc-parser
 '(peg-parser
    [(a A) (b B)]
    (S
      [((& a (! "b")) (+ "a") b (! "c")) #t])
    (A
      [("a" (? a) "b") #t])
    (B
      [("b" (? b) "c") #t])))

(define sexpr-parser
 '(peg-parser
    [(d Datum)]
    (Sexpr
      [("(" (* datum (? ws)) ")") datum]
      [("(" (+ datum1 (? ws)) "." datum2 ")") `(,@datum1 . ,datum2)])
    (Datum
      [bool bool]
      [num num]
      [char char]
      [str str]
      [sym sym]
      [ls ls]
      [vec vec])
    (Boolean
      ["#t" #t]
      ["#f" #f])
    (Number
      [bin bin]
      [oct oct]
      [dec dec]
      [hex hex])
    (Character
      [("#\\" anychar) anychar]
      ["#\\newline" #\newline]
      ["#\\space" #\space])
    (String
      [("\"" (* schar) "\"") (list->string schar)])
    (StringCharacter
      ["\\\"" #\"]
      ["\\\\" #\\]
      [((! (/ #\" #\\)) anychar) anychar])
    (Symbol
      [ident ident])
    (Identifier
      [(init (* subseq)) (string->symbol (list->string `(,init ,@subseq)))]
      ["+" '+]
      ["-" '-]
      ["..." '...])))

(delete-file "expr.ss")
(with-output-to-file "expr.ss"
  (lambda ()
    (pretty-print
      (expand/optimize abc-parser))))

(define do-test
  (lambda (parser input expected?)
    (let ([test-string (string->list input)])
      (define generator
        (lambda ()
          (if (null? test-string)
            (eof-object)
            (let ([c (car test-string)])
              (set! test-string (cdr test-string))
              c))))
      (let ([r (parser generator)])
        (expected? r)))))

(define do-tests
  (lambda ()
    (do-test (eval expr-parser) "(22+122)*(3+2)" (lambda (v) (integer? v)))
    (do-test (eval abc-parser) "aaabbbccc" (lambda (v) v))
))

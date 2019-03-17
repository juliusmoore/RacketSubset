#lang racket

;added by justin O, probably will not work well
;check that car is not a pair and cdr is a pair that contains only one item
(define (eval_add x)
  (if (pair? x)
    (+ (car x) (cdr x))
    (error "Error in function addition, x is not a pair.")))

(define (eval_sub x)
  (if (pair? x)
    (- (car x) (car (cdr x)))
    (error "Error in function subtraction, x is not a pair.")))

(define (eval_mult x)
  (if (pair? x)
    (* (car x) (car (cdr x)))
    (error "Error in function multiplication, x is not a pair.")))

(define (eval_div x)
  (if (pair? x)
    (/ (car x) (car (cdr x)))
    (error "Error in function division, x is not a pair.")))


(define (startEval rkt) (execute rkt '(())))

(define (eval_quote rkt state) (quote rkt))

(define (getPairWithKey key list)
  (if (pair? list)
      (if (null? list)
          (error key "is undefined")
          (let ([candidate (car list)])
            (if (pair? candidate)
                (if (equal? (car candidate) key)
                    (cdr candidate)
                    (getPairWithKey key (cdr list)))
                (error "The state has been corrupted by an unparist"))))
      (error "The state is not a pair!"))
  )

(define (execute_var func state)
 (if (pair? func)
     (error "THis is not a variable : " func " in state : " state)
     (execute getPairWithKey(func state) state)))

(define (execute rkt state) 
  (if (pair? rkt)
      (if (null? rkt)
          '()
          (let ([func (car rkt)] [defi (cdr rkt)])
            (if (pair? func)
                (error "You Fucked Up : " func "should not be a list. Found in: " rkt) ;insert lambdas here
                (cond
                  [(equal? func '+) (eval_add defi state)]
                  [(equal? func '-) (eval_sub defi state)]
                  [(equal? func '*) (eval_mult defi state)]
                  [(equal? func '/) (eval_div defi state)]
                  [(equal? func '<) (eval_LT defi state)]
                  [(equal? func '<=) (eval_LTE defi state)]
                  [(equal? func '=) (eval_E defi state)]
                  [(equal? func '>) (eval_GT defi state)]
                  [(equal? func '>=) (eval_GTE defi state)]
                  [(equal? func 'car) (eval_car defi state)]
                  [(equal? func 'cdr) (eval_cdr defi state)]
                  [(equal? func 'cons) (eval_cons defi state)]
                  [(equal? func 'pair?) (eval_pair? defi state)]
                  [(equal? func 'if) (eval_if defi state)]
                  [(equal? func 'lambda) (eval_lambda defi state)]
                  [(equal? func 'let) (eval_let defi state)]
                  [(equal? func 'letrec) (eval_letrec defi state)]
                  [(equal? func 'quote) (eqval_quote defi state)] ;quotes
                  [else (execute_var func state)])
                )))
      (if (number? rkt)
          rkt
          (error "You Fucked Up (NaN): " rkt " in state : " state)
          )
      )
  )
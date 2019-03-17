#lang racket

(define (startEval rkt) (execute rkt '(())))

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
[(equal? func 'equal?) (eval_equal defi state)]
                  [(equal? func '=) (eval_eq defi state)]
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
                  [(equal? func 'quote) (eval_quote defi state)] ;quotes
                  [else (execute_var func state)])
                )))
      (if (number? rkt)
          rkt
          (error "You Fucked Up (NaN): " rkt " in state : " state)
          )
      )
  )


(define (eval_add x state)
  (if (pair? x)
    (if(pair? (car(cdr x)))
      (+ (car x) (execute(car(cdr x) state)))
      (+ (car x) (car(cdr x))))
    (error "Error in function eval_add, x is not a pair.")))

(define (eval_sub x state)
  (if (pair? x)
    (if(pair? (car(cdr x)))
      (- (car x) (execute(car(cdr x) state)))
      (- (car x) (car(cdr x))))
    (error "Error in function eval_sub, x is not a pair.")))

(define (eval_mult x state)
  (if (pair? x)
    (if(pair? (car(cdr x)))
      (* (car x) (execute(car(cdr x) state)))
      (* (car x) (car(cdr x))))
    (error "Error in function eval_mult, x is not a pair.")))

(define (eval_div x state)
  (if (pair? x)
    (if(pair? (car(cdr x)))
      (/ (car x) (execute(car(cdr x) state)))
      (/ (car x) (car(cdr x))))
    (error "Error in function eval_div, x is not a pair.")))


(define (eval_equal x state)
  (if (pair? x)
    (if(pair? (car(cdr x)))
      (equal? (car x) (execute(car(cdr x) state)))
      (equal? (car x) (car(cdr x))))
    (error "Error in function eval_equal, x is not a pair.")))

(define (eval_eq x state)
  (if (pair? x)
    (if(pair? (car(cdr x)))
      (= (car x) (execute(car(cdr x) state)))
      (= (car x) (car(cdr x))))
    (error "Error in function eval_eq, x is not a pair.")))

;staying the same or increasing
(define (eval_LTE x state)
  (if (pair? x)
    (if(pair? (car(cdr x)))
      (<= (car x) (execute(car(cdr x) state)))
      (<= (car x) (car(cdr x))))
    (error "Error in function eval_LTE, x is not a pair.")))

;increasing order
(define (eval_LT x state)
  (if (pair? x)
    (if(pair? (car(cdr x)))
      (< (car x) (execute(car(cdr x) state)))
      (< (car x) (car(cdr x))))
    (error "Error in function eval_LT, x is not a pair.")))

;decreasing order or staying the same
(define (eval_GTE x  state)
(if (pair? x)
  (if(pair? (car(cdr x)))
    (>= (car x) (execute(car(cdr x) state)))
    (>= (car x) (car(cdr x))))
  (error "Error in function eval_GTE, x is not a pair.")))

;decreasing order
(define (eval_GT x  state)
  (if (pair? x)
    (if(pair? (car(cdr x)))
      (> (car x) (execute(car(cdr x) state)))
      (> (car x) (car(cdr x))))
    (error "Error in function eval_GT, x is not a pair.")))

(define (eval_car x state)
  (if (pair? x)
    (car x)
    (error "Error in function eval_car, x is not a pair.")))

(define (eval_cdr x state)
  (if (pair? x)
    (cdr x)
    (error "Error in function eval_cdr, x is not a list.")))

(define (eval_cons x state)
  (if (and  (pair? x) (pair? (cdr x)))
    (cons car x cdr x)
    (error "Error in function eval_cons, x is not a pair.")))


(define (eval_pair? x state) (if (pair? x) #t #f))

(define (eval_list? x state) (if (list? x) #t #f))

(define (eval_null? x state) (if (null? x) #t #f))

(define (eval_num? x state) (if (number? x) #t #f))

(define (eval_quote x state) (quote x) )


(define (eval_if x state)
  (if (list? x)
    (if (list? (first x))
      (if(execute((first x) state))
        (execute ((second x) state))
        (execute ((third x) state)))
      (error "everything is on fire in eval_if."))
    (error "everything is on fire in eval_if.")))


;Only implement the plain lambda syntax without support
;for keyword or optional arguments. Assume there is only one expression in the body.
; rational ((lambda{first} (args) {second} ---body---- {rest or three}){car} {cdr})
(define (eval_lambda x state)
(if (and(and(pair? car(second(x))) (pair? car(third(x)))) (pair? cdr(x)))
    ((lambda (execute(second(car(x))), state)  (execute(third(car(x)), state)))
     (execute((cdr(x)) state)))
(error "Justin's lambda function broke again. what an idiot.")))

(define (eval_let x state) "empty")

(define (eval_letrec x state)"empty")

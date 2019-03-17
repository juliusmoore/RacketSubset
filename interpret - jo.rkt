#lang racket
(define (execute x state) "skill")
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
;((lambda{first} (args) {second} ---body---- {rest(three)}){car} {cdr})
(define (eval_lambda x state)
(if (and(and(pair? car(second(x))) (pair? car(third(x)))) (pair? cdr(x))) 
    ((lambda (execute(car(second(x))), state)  (execute(car(third(x)), state)))
     (execute((cdr(x)) state)))
(error "Justin's lambda function broke again.")))

(define (eval_let x state) "empty")

(define (eval_letrec x state)"empty")
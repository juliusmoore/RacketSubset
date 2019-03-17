#lang racket

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
  (if (list? x)
    car x
    (error "Error in function eval_car, x is not a pair.")))

(define (eval_cdr x state)
  (if (list? x)
    cdr x
    (error "Error in function eval_cdr, x is not a list.")))

(define (eval_cons x state)
  (if (and  (pair? x) (pair? (cdr x)))
    (cons car x cdr x)
    (error "Error in function eval_cons, x is not a pair.")))


(define (eval_pair? x state) (if (pair? x) #t #f))

(define (eval_list? x state) (if (list? x) #t #f))

(define (eval_null? x state) (if (null? x) #t #f))

(define (eval_num? x state) (if (number? x) #t #f))

(define (eval_lambda x state) '"fricken empty")

(define (eval_let x state) '"fricken empty")

(define (eval_letrec x state) '"fricken empty")

(define (execute rkt state) '"fexe")

#lang racket


(define (startEval rkt) (execute rkt '(())))

(define (getPairWithKey key state)
  (if (pair? state)
      (if (null? state)
          (error key "is undefined")
          (let ([candidate (car state)])
            (if (pair? candidate)
                (if (equal? (car candidate) key)
                    (cdr candidate)
                    (getPairWithKey key (cdr state)))
                (error "The state has been corrupted by an unparist"))))
      (error "The state is not a pair!"))
  )

(define (execute_var func defi state)
 (if (pair? func)
     (error "THis is not a variable : " func " in state : " state)
     (execute (cons getPairWithKey(func state) '(defi)) state)))

(define (execute rkt state)
  (if (pair? rkt)
      (if (null? rkt)
          '()
          (let ([func (car rkt)] [defi (cdr rkt)])
            (if (pair? func)
                (eval_lambda rkt state)
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
                  [(equal? func 'fact) (eval_fact defi state)]
                  [(equal? func 'quote) (eval_quote defi state)] ;quotes
                  [else (execute_var func defi state)])
                )))
      (if (number? rkt)
          rkt ;only if its a literal, or a representation of a literal, is this "ok"
          (let ([val (getPairWithKey rkt state)])
            (if (number? val)
                val
                (error "You Fucked Up: Attempted to use " rkt " defined using a list as a literal. state : " state))
            )
          )
      )
  )

;this function adds two values 
(define (eval_add x state)
    (if (null? x)
     (error "input is null for eval_add")
     (if (pair? (car x))
         (if(pair? (car(cdr x)))
            (+ (execute(car x) state) (execute(car(cdr x)) state))
            (+ (execute(car x) state) (car(cdr x))))
         (if(pair? (car(cdr x)))
            (+ (car x) (execute(car(cdr x)) state))
            (+ (car x) (car(cdr x)))))))

;this function subtracts  two values 
(define (eval_sub x state)
    (if (null? x)
     (error "input is null for eval_sub")
     (if (pair? (car x))
         (if(pair? (car(cdr x)))
            (- (execute(car x) state) (execute(car(cdr x)) state))
            (- (execute(car x) state) (car(cdr x))))
         (if(pair? (car(cdr x)))
            (- (car x) (execute(car(cdr x)) state))
            (- (car x) (car(cdr x)))))))

;this function multiplies  two values 
(define (eval_mult x state)
    (if (null? x)
     (error "input is null for eval_mult")
     (if (pair? (car x))
         (if(pair? (car(cdr x)))
            (* (execute(car x) state) (execute(car(cdr x)) state))
            (* (execute(car x) state) (car(cdr x))))
         (if(pair? (car(cdr x)))
            (* (car x) (execute(car(cdr x)) state))
            (* (car x) (car(cdr x)))))))

;this function divides two values 
(define (eval_div x state)
    (if (null? x)
     (error "input is null for eval_div")
     (if (pair? (car x))
         (if(pair? (car(cdr x)))
            (/ (execute(car x) state) (execute(car(cdr x)) state))
            (/ (execute(car x) state) (car(cdr x))))
         (if(pair? (car(cdr x)))
            (/ (car x) (execute(car(cdr x)) state))
            (/ (car x) (car(cdr x)))))))

;this function checks to see if two values are equal, but with equal?.
(define (eval_equal x state)
    (if (null? x)
     (error "input is null for eval_equal")
     (if (pair? (car x))
         (if(pair? (car(cdr x)))
            (equal? (execute(car x) state) (execute(car(cdr x)) state))
            (equal? (execute(car x) state) (car(cdr x))))
         (if(pair? (car(cdr x)))
            (equal? (car x) (execute(car(cdr x)) state))
            (equal? (car x) (car(cdr x)))))))

;this function checks to see if two values are equal, but with =.
(define (eval_eq x state)
    (if (null? x)
     (error "input is null for eval_eq")
     (if (pair? (car x))
         (if(pair? (car(cdr x)))
            (= (execute(car x) state) (execute(car(cdr x)) state))
            (= (execute(car x) state) (car(cdr x))))
         (if(pair? (car(cdr x)))
            (= (car x) (execute(car(cdr x)) state))
            (= (car x) (car(cdr x)))))))

; this function checks to see if left is less then or equal to right.
(define (eval_LTE x state)
    (if (null? x)
     (error "input is null for eval_LTE")
     (if (pair? (car x))
         (if(pair? (car(cdr x)))
            (<= (execute(car x) state) (execute(car(cdr x)) state))
            (<= (execute(car x) state) (car(cdr x))))
         (if(pair? (car(cdr x)))
            (<= (car x) (execute(car(cdr x)) state))
            (<= (car x) (car(cdr x)))))))

; this function checks to see if left is less then to right.
(define (eval_LT x state)
    (if (null? x)
     (error "input is null for eval_LT")
     (if (pair? (car x))
         (if(pair? (car(cdr x)))
            (< (execute(car x) state) (execute(car(cdr x)) state))
            (< (execute(car x) state) (car(cdr x))))
         (if(pair? (car(cdr x)))
            (< (car x) (execute(car(cdr x)) state))
            (< (car x) (car(cdr x)))))))

;This function checks to see if left is greater then or equal to right.
(define (eval_GTE x  state)
    (if (null? x)
     (error "input is null for eval_GTE")
     (if (pair? (car x))
         (if(pair? (car(cdr x)))
            (>= (execute(car x) state) (execute(car(cdr x)) state))
            (>= (execute(car x) state) (car(cdr x))))
         (if(pair? (car(cdr x)))
            (>= (car x) (execute(car(cdr x)) state))
            (>= (car x) (car(cdr x)))))))

;This function checks to see if left is greater then to right.
(define (eval_GT x  state)
    (if (null? x)
     (error "input is null for eval_GT")
     (if (pair? (car x))
         (if(pair? (car(cdr x)))
            (> (execute(car x) state) (execute(car(cdr x)) state))
            (> (execute(car x) state) (car(cdr x))))
         (if(pair? (car(cdr x)))
            (> (car x) (execute(car(cdr x)) state))
            (> (car x) (car(cdr x)))))))

;function outputs first value of a list.
(define (eval_car x state)
  (if (null? x)
     (error "input is null for Eval_Car")
     (if (pair? x)
         (caadr(append* x)) ; car car cdr
         (if (pair? (execute(x) state) ) 
             (car(execute(x) state))
             (error "input is not a pair for eval_car.")))))

;function outputs rest of the values of a list.
(define (eval_cdr x state)
  (if (null? x)
     (error "input is null for eval_cdr")
     (if (list? x)        
         (cdadr(append* x)) ;cdr car cdr
         (if (list? (execute(x) state) ) 
             (cdr(execute(x) state))
             (error "input is not a list.")))))

(define (eval_cons x state)
  (if (and  (pair? x) (pair? (cdr x)))
    (cons (car x) (car (cdr x)))
    (error "Error in function eval_cons, x is not a pair.")))

;logical operators
(define (eval_and x state)
  (if (pair? x)
    (if(pair? (car(cdr x)))
      (and (car x) (execute(car(cdr x) state)))
      (and (car x) (car(cdr x))))
    (if(pair? (car x))
      (and (execute(car x)state) (execute(car(cdr x) state)))
      (and (execute(car x)state) (car(cdr x))))))

(define (eval_or x state)
  (if (pair? x)
    (if(pair? (car(cdr x)))
      (or (car x) (execute(car(cdr x) state)))
      (or (car x) (car(cdr x))))
    (if(pair? (car x))
      (or (execute(car x) state) (execute(car(cdr x) state)))
      (or (execute(car x) state) (car(cdr x))))))

(define (eval_xor x state)
  (if (pair? x)
    (if(pair? (car(cdr x)))
      (xor (car x) (execute(car(cdr x) state)))
      (xor (car x) (car(cdr x))))
    (if(pair? (car x))
      (xor (execute(car x) state) (execute(car(cdr x) state)))
      (xor (execute(car x) state) (car(cdr x))))))

(define (eval_not x state)
  (if (pair? x)
    (if(pair? (car(cdr x)))
      (not (car x) (execute(car(cdr x) state)))
      (not (car x) (car(cdr x))))
    (if(pair? (car x))
      (not (execute(car x)) (execute(car(cdr x) state)))
      (not (execute(car x)) (car(cdr x))))))

(define (eval_pair? x state) (if (pair? (car x)) #t #f))

(define (eval_list? x state) (if (list? (car x)) #t #f))

(define (eval_null? x state)
  (if (null? x)
      #t
      (if (null? (car x) )
          #t
          #f)))

(define (eval_num? x state) (if (number? x) #t #f))
;this function produces a constant, also adds the char' infront of stuff
(define (eval_quote x state) (quote x) )


(define (eval_if x state)
  (if (list? x)
    (if (list? (first x))
      (let ([a (execute(first x) state)])
      (if(equal? a #t)
        (execute (second x) state)
        (execute (third x) state)))
      (error "everything is on fire in eval_if."))
    (error "everything is on fire in eval_if.")))

;this function finds the factorial of parameter x
(define (_fact  x)
  (if(pair? x)
     (let ([y (car x)])
       (letrec ([xy
                 (lambda (y)
                   (if (= y 0) 
                       1
                       (* y (_fact (- y 1)))))]) (xy y)))
     (let ([y  x])
       (letrec ([xy
                 (lambda (y)
                   (if (= y 0) 
                       1
                       (* y (_fact (- y 1)))))]) (xy y)))))
;this 
(define (eval_fact x state)    
   (if (list?  (car x))
         (_fact(execute (car x) state))
         (_fact x)))
         
;(define (eval_find_Replace))
;Only implement the plain lambda syntax without support
;for keyword or optional arguments. Assume there is only one expression in the body.
; rational ((lambda{first} (args) {second} ---body---- {rest or three}){car} {cdr})
;use getpairWithKey
;()' (x)' (x .... n) <-args
; state 
(define (eval_lambda funct args state)
  (if(list? (second (car x))) ;if params are list
  (cond [(or (not(list? (second(car x)))) (equal? 1 (length(second(car x))))) ; If length is one do simple version of the lambda algorithm
      (let ([a (car(second(car x)))] ; first param
            [c (cadr x)]) ; car cdr  value of first param
       ;add if lambda
       (if (list? c)         
           (execute (third(car x)) state)
          (if (list? (third(car x)))
           (if  (equal? a (second(third(car x))))
                (if (equal? 2 (length(third(car x))))
                   ( execute (append (list (first(third(car x)))) (list (execute c state))) state) 
                   ( execute (append (list (first(third(car x)))) (list (execute c state)) (list (third(third(car x))))) state))
                   ( execute (append (list (first(third(car x)))) (list (second(third(car x)))) (list (execute c state))) state))
           c)))]
      ;else this is the more complicated version
      [(equal? 2 (length(second(car x)))) (let ;length 2 algorithm 
        ([a (car(second(car x)))] ; first param
           [b (car(cdr(second(car x))))] ; second param
           [c (cadr x)] ;car cdr  value of first param
           [d (caddr x)]) ;car cdr cdr ; value of second param
           (if (list? c)         
           (execute (third(car x)) state)
           (if  (equal? a (second(third(car x))))  
                (execute (append (list (first(third(car x)))) (list (execute c state)) (list (execute d state))) state)
                (execute (append (list (first(third(car x)))) (list (execute d state))) (list (execute c state)) state))))])
(error "No parameters for lambda function")))

           
  ;( execute (append (list (first(third(car x)))) (list (second(third(car x)))) (list c)) state))

(define (addToState vars state)
  (if (and (pair? vars) (not (null? vars)))
      (let ([addition (car vars)] [rest (cdr vars)])
           (if (null? rest)
               (cons addition (state))
               (cons addition ((addToState rest state)))))
      (error "You have caused a calamity: addToState on " vars " state : " state)))

(define (produceExecutedPair p state)
  (if (and (pair? p) (not (null? p)))
  (cons (car p) (execute (car (cdr p)) state))
  (error "You have caused a calamity: produceExecutedPair on " p " state : " state)))

(define (evalToState vars state)
  (if (and (pair? vars) (not (null? vars)))
      (let ([addition (produceExecutedPair (car vars) state)] [rest (cdr vars)])
        (if (null? rest)
            (cons addition (cons state '()))
               (cons addition ((evalToState rest state)))))
      (error "You have caused a calamity: evalToState on " vars "state : " state)))

 ;evaluate then store
(define (eval_let x state)
  (if (and (pair? x) (equal? (length x) 2))
      (execute (car (cdr x)) (evalToState (car x) state)) 
      (error "You have caused a calamity: let on " x " state : " state)))

 ;store then evaluate
(define (eval_letrec x state)
  (if (and (pair? x) (equal? (length x) 2))
      (execute (car (cdr x)) (addToState (car x) state))
      (error "You have caused a calamity: letrec on " x " state : " state)))
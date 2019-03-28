#lang racket

(define (startEval rkt) (execute rkt '()))


(define (getPairWithKey key state)
  (if (pair? state)
      (if (null? state)
          (error key "is undefined" state)
          (let ([candidate (car state)])
            (if (pair? candidate)
                (if (equal? (car candidate) key)
                    (cdr candidate)
                    (getPairWithKey key (cdr state)))
                (error "The state has been corrupted by an unparist" state))))
      (error key " is undefined in " state))
  )

;Create a bug! Pass in an empty list in defi that was generated instead of being a real argument. We will then execute defi 
;(define (execute_var func defi state)
;  (print (list "state : " state))
 ; (if (pair? func)
;     (error "THis is not a variable : " func " in state : " state)
 ;    (if (pair? defi)
;     (if (null? defi)
  ;       (execute (getPairWithKey func state) state)
 ;        (execute (cons (getPairWithKey func state) defi) state))
;     (execute (list (getPairWithKey func state) defi) state))))

(define (execute rkt state)
  (if (pair? rkt)
      (if (null? rkt)
          '()
          (let ([func (car rkt)] [defi (cdr rkt)])
            (if (pair? func)
                (if (equal? (car func) 'lambda)
                    (eval_lambda func defi state)
                    (error "Goats don't go baaah : We have not found a lambda in " rkt " state : " state))
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
                  [(equal? func 'lambda) (eval_lambda rkt '() state)]
                  [else
                   (execute (cons (getPairWithKey func state) defi) state)
                   ])
                )))
      (if (number? rkt)
          rkt ;only if its a literal, or a representation of a literal, is this "ok"
          (let ([val (getPairWithKey rkt state)])
            (if (number? val)
                val
                (execute val state) ; chain of variables? -> will catch error in next getPairWithKey
            )
          )
      )
  )
  )

;this function adds two values 
(define (eval_add x state)
  (if (null? x)
       ;option 4 is to replace the body of (if (null? x)) with these 3 lines, and fix getPairWithKey.
              ; (if (pair?  x) ; line 1
              ;    (+ (execute(car x) state) (execute(car(cdr x)) state)) ; line 2
              ;   (raise-arguments-error 'eval_add "not a pair" "first parameter" x );line 3
      (error "input is null for eval_add")
      (if (or (number? (car x)) (number? (car (cdr x))))
          (if (pair? (car x))
             
             
              (if(pair? (car(cdr x)))
                 (+ (execute(car x) state) (execute(car(cdr x)) state)) ;if rhs and lhs are pairs execute is called
                 (+ (execute(car x) state) (car(cdr x)))) ;if rhs is a pair but lhs is not execute is called for rhs.
              (if(pair? (car(cdr x)))
                 (+ (car x) (execute(car(cdr x)) state)) ;if  rhs is not a pair but lhs is a pair execute is called for lhs
                 (+ (car x) (car(cdr x))))) ; if neither rhs or lhs are pairs then execute is not called.
          ; option 1 (error x " does not contain a number.")
          ; option 2
          (if (pair? (car state))
              (if (pair? (car (cdr state)))
                  ;checks to see if there are values in the state.
                  ;if there are values in the first sublist and second sublist in state.
                  ;then we treate the first value as lhs and second as rhs.
                  (if(not(equal? (caar state) (car (car(cdr state)))))
                     (+   (cdr (car state)) (cdr (car (cdr state))))
                     (raise-arguments-error 'eval_add
                                            "Variables in state are the same."
                                            "first"(car (car(cdr state)))
                                            "second" (car (car(cdr state)))
                                            "full state" state))
                  (error "error in the eval_add function value of x:" x " the value of state:" state))
              (error "error in the eval_add function value of x:" x " the value of state:" state)))  ))

         ; option 3
         ;(+ (getPairWithKey rkt state))
       

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

;this function multiply  two values 
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


(define (eval_lambda lamb args state)
  (eval_lambda_new lamb args state)
  )

;bug in design:
; I have chosen not to deal with this format (lambda x x) (only deal with arguments as list (lambda (x) x) ...)
;we could receive:
; 0 to infinity arguments to the lambda
; 0 to infinity parameters given to the lambda
;we can result in:
;a partially evaluated lambda
;no action (nothing to evaluate) (no parameters to evaluate the lambda on)
;a fully evaluated lambda that is executed (#arguments = #parameters)
;a fully evaluated lambda that is executed, then dumped into a longer list (more parameters given than there are arguments in the lambda)
(define (eval_lambda_new lamb param state)
  (let ([arg (cadr lamb)][body (caddr lamb)])
    (if (null? arg)
        (if (null? param)
            (execute body state) ; simple lambda
            (execute (cons (execute body state) param) state)) ; lambda may have returned a lambda, can keep trying
        (if (null? param)
            lamb ;(error "We would like to return a partially evaluated lambda, but to do that you cannot execute us! partial : " lamb " state: " state); sadly racket requires we crash here (argument mismatch)
            (let ([keyValueList (cons (car arg) (cons (car param) '()))] [leftoverArg (cdr arg)] [leftoverParam (cdr param)])
              (eval_lambda_new (cons 'lambda (cons leftoverArg
                                                     (cons (list 'let (cons keyValueList '()) body) '())
                                                   )) leftoverParam state)
              )))
    )
  )
                     



;Append all the key value pairs in a list into the state ( (x . 3) (y . 5) ) -> state
(define (addToState vars state)
  (if (and (pair? vars) (not (null? vars)))
      (let ([addition (car vars)] [rest (cdr vars)])
           (if (null? rest)
               (cons addition (state))
               (cons addition ((addToState rest state)))))
      (error "You have caused a calamity: addToState on " vars " state : " state)))

;Perform the process of creating a (name . value) pair where value has been evaluated to the extent possible.
;Arguments:
; p : the (name . value) pair whose value should be evaluated
; state : the current state of the stack
; Returns : 
(define (produceExecutedPair p state)
  (if (and (pair? p) (not (null? p)))
  (cons (car p) (execute (car (cdr p)) state))
  (error "You have caused a calamity: produceExecutedPair on " p " state : " state)))

;Evaluate the values in a list of (name . value) pairs and store the evaluations with their names as a state
;Arguments:
; vars : the list of name-value pairs to have their values evalutated
; state : the state of the stack to append to
;Returns: A state/stack with the name-value pairs added to the front/top
(define (evalToState vars state)
  (if (and (pair? vars) (not (null? vars)))
      (let ([addition (produceExecutedPair (car vars) state)] [rest (cdr vars)])
        (if (null? rest)
            (cons addition (cons state '()))
               (cons addition ((evalToState rest state)))))
      (error "You have caused a calamity: evalToState on " vars "state : " state)))

 ;evaluate then store
;Performs the let function
;Arguments:
; x : the list of arguments to the function
; state : the curret state of the stack
;Returns : Follows the path of the Racket.
(define (eval_let x state)
  (if (and (pair? x) (equal? (length x) 2))
      (execute (car (cdr x)) (evalToState (car x) state)) 
      (error "You have caused a calamity: let on " x " state : " state)))

 ;store then evaluate
;Performs the letrec function of racket.
;Arguments:
; x : the list of arguments to the function
; state : the curret state of the stack
(define (eval_letrec x state)
  (if (and (pair? x) (equal? (length x) 2))
      (execute (car (cdr x)) (addToState (car x) state))
      (error "You have caused a calamity: letrec on " x " state : " state)))
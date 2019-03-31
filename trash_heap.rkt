#lang racket

; Racket Interpreter
; Course Project for Programming Languages - CPSC 3740 - Spring 2019 - Prof. Howard Cheng
; This interpreter was written by Julius Moore 001167698 and Justin Onoferychuk 001212560

(define (startEval rkt) (execute rkt '()))


(define (getPairWithKey key state)
  ;(println (list "Called getPairWithKey: " key " : in state : " state))
  (if (null? state)
      (error "Oops: undefined : " key)
      (if (pair? state)
          (let ([candidate (car state)] [continuation (cdr state)])
            (if (equal? (car candidate) key)
                (cdr candidate); we are storing pure pairs not lists
                (getPairWithKey key continuation)))
      (error "Oops: The state cannot be a literal: " state))))


(define (execute rkt state)
  (println (list rkt ':::::: state))
  (if (pair? rkt)
      (if (null? rkt)
          '()
          (let ([func (car rkt)] [defi (cdr rkt)])
            (if (pair? func)
                (if (equal? (car func) 'lambda)
                    (eval_lambda func defi '() state)
                    (execute (cons (execute func state) defi) state))
                    ;(error "Goats don't go baaah : We have not found a lambda in " rkt " state : " state)) ; change thi
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
                  [(equal? func 'let) (eval_let defi state)]
                  [(equal? func 'letrec) (eval_letrec defi state)]
                  [(equal? func 'quote) (eval_quote defi state)] ;quotes
                  [(equal? func 'lambda) (eval_lambda rkt '() '() state)]
                  [else
                   ;(println (list " rkt : " rkt " : func : " func " : defi : " defi " : state : " state))
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

;evaluates + symbol
;adds two value 
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns : a added value.
;this function adds two values 
(define (eval_add x state)
  (if (not (null? x))
       ;must fix getPairWithKey.
       (if (pair?  x) ; line 1
           (+ (execute(car x) state) (execute(car(cdr x)) state)) ; line 2
           (raise-arguments-error 'eval_add "not a pair" "first parameter" x ))
       (raise-arguments-error 'eval_add "not not null" "first parameter" x)));line 3
 
       
;evaluates - symbol
;subtracts two value 
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns : a subtracted  value.
;this function subtracts  two values 
(define (eval_sub x state)
      (if (not (null? x))
       ;must fix getPairWithKey.
        (if (pair?  x) ; line 1
            (- (execute(car x) state) (execute(car(cdr x)) state)) ; line 2
            (raise-arguments-error 'eval_sub "not a pair" "first parameter" x ))
        (raise-arguments-error 'eval_sub "not not null" "first parameter" x)));line 3

;evaluates * symbol
;multiplies two value 
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns : a multiplied value.
;this function multiply  two values 
(define (eval_mult x state)
    (if (not(null? x))
      (if (pair?  x) ; line 1
          (* (execute(car x) state) (execute(car(cdr x)) state)) ; line 2
          (raise-arguments-error 'eval_mult "not a pair" "first parameter" x ))
      (raise-arguments-error 'eval_mult "not not null" "first parameter" x)));line 3

;evaluates / symbol
;divides two value 
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns : a divided value.
;this function divides two values 
(define (eval_div x state)
   (if (not(null? x))
     (if (pair?  x) ; line 1
         (/ (execute(car x) state) (execute(car(cdr x)) state)) ; line 2
         (raise-arguments-error 'eval_div "not pair" "first parameter" x ))
     (raise-arguments-error 'eval_div "not not null" "first parameter" x)));line 3

;evaluates  equal, but with equal?
;returns true if the lhs is equal rhs
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns : #t or #f.
;this function checks to see if two values are equal, but with equal?.
(define (eval_equal x state)
   (if (not(null? x))
     (if (pair?  x) ; line 1
         (equal? (execute(car x) state) (execute(car(cdr x)) state)) ; line 2
         (raise-arguments-error 'eval_div "not pair" "first parameter" x ))
     (raise-arguments-error 'eval_div "not not null" "first parameter" x)));line 3

;evaluates  equal, but with =
;returns true if the lhs is equal rhs
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns : #t or #f.
;this function checks to see if two values are equal, but with =.
(define (eval_eq x state)
   (if (not(null? x))
     (if (pair?  x) ; line 1
         (= (execute(car x) state) (execute(car(cdr x)) state)) ; line 2
         (raise-arguments-error 'eval_div "not pair" "first parameter" x ))
     (raise-arguments-error 'eval_div "not not null" "first parameter" x)));line 3

;evaluates less then or equal
;returns true if the lhs is less then or equal rhs
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns : #t or #f.
; this function checks to see if left is less then or equal to right.
(define (eval_LTE x state)
   (if (not(null? x))
     (if (pair?  x) ; line 1
         (<= (execute(car x) state) (execute(car(cdr x)) state)) ; line 2
         (raise-arguments-error 'eval_div "not pair" "first parameter" x ))
     (raise-arguments-error 'eval_div "not not null" "first parameter" x)));line 3

;evaluates less then
;returns true if the lhs is less then rhs
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns : #t or #f.
; this function checks to see if left is less then to right.
(define (eval_LT x state)
   (if (not(null? x))
     (if (pair?  x) ; line 1
         (< (execute(car x) state) (execute(car(cdr x)) state)) ; line 2
         (raise-arguments-error 'eval_div "not pair" "first parameter" x ))
     (raise-arguments-error 'eval_div "not not null" "first parameter" x)));line 3

;evaluates greater then or equal
;returns true if the lhs is greater then or equal rhs
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns : #t or #f.
;This function checks to see if left is greater then or equal to right.
(define (eval_GTE x  state)
    (if (not(null? x))
     (if (pair?  x) ; line 1
         (>= (execute(car x) state) (execute(car(cdr x)) state)) ; line 2
         (raise-arguments-error 'eval_div "not pair" "first parameter" x ))
     (raise-arguments-error 'eval_div "not not null" "first parameter" x)));line 3

;evaluates greater then
;returns true if the lhs is greater then rhs
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns : #t or #f.
;This function checks to see if left is greater then to right.
(define (eval_GT x  state)
   (if (not(null? x))
     (if (pair?  x) ; line 1
         (> (execute(car x) state) (execute(car(cdr x)) state)) ; line 2
         (raise-arguments-error 'eval_div "not pair" "first parameter" x ))
     (raise-arguments-error 'eval_div "not not null" "first parameter" x)));line 3

;evaluates car
;returns first element of a pair
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns : first element is returned.
;function outputs first value of a list.
(define (eval_car x state)
  (if (or (null? x) (not (pair? x)))
      (error "." x " is not a list : state : " state)
      (car (execute (car x) state))))

;evaluates cdr
;returns second element of a pair
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns : second element is returned.
;function outputs rest of the values of a list.
(define (eval_cdr x state)
  (if (or (null? x) (not (pair? x)))
      (error "." x " is not a list : state : " state)
      (cdr (execute (car x) state))))

;evaluates cons
;joins two values into a pair
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns : a pair.
(define (eval_cons x state)
  (if (and  (pair? x) (pair? (cdr x)))
    (cons (execute (car x) state) (execute (car (cdr x)) state))
    (error "Error in function eval_cons, x is not a pair.")))


;logical operators

;evaluates and
;Performs the and function
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns :    #t or  #f.
(define (eval_and x state)
  (if (pair? x)
    (if(pair? (car(cdr x)))
      (and (car x) (execute(car(cdr x) state)))
      (and (car x) (car(cdr x))))
    (if(pair? (car x))
      (and (execute(car x)state) (execute(car(cdr x) state)))
      (and (execute(car x)state) (car(cdr x))))))

;evaluates or
;Performs the or function
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns :    #t or  #f.
(define (eval_or x state)
  (if (pair? x)
    (if(pair? (car(cdr x)))
      (or (car x) (execute(car(cdr x) state)))
      (or (car x) (car(cdr x))))
    (if(pair? (car x))
      (or (execute(car x) state) (execute(car(cdr x) state)))
      (or (execute(car x) state) (car(cdr x))))))

;evaluates xor
;Performs the xor function
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns :    #t or  #f.
(define (eval_xor x state)
  (if (pair? x)
    (if(pair? (car(cdr x)))
      (xor (car x) (execute(car(cdr x) state)))
      (xor (car x) (car(cdr x))))
    (if(pair? (car x))
      (xor (execute(car x) state) (execute(car(cdr x) state)))
      (xor (execute(car x) state) (car(cdr x))))))

;evaluates not
;Performs the not function
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns :   not #t or not #f.
(define (eval_not x state)
  (if (pair? x)
    (if(pair? (car(cdr x)))
      (not (car x) (execute(car(cdr x) state)))
      (not (car x) (car(cdr x))))
    (if(pair? (car x))
      (not (execute(car x)) (execute(car(cdr x) state)))
      (not (execute(car x)) (car(cdr x))))))

;evaluates pair?
;Performs the pair? function
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns :  #t or #f.
(define (eval_pair? x state) (if (pair? (car x)) #t #f))

;evaluates list?
;Performs the list? function
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns :  #t or #f.
(define (eval_list? x state) (if (list? (car x)) #t #f))

;evaluates null?
;Performs the null? function
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns :  #t or #f.
(define (eval_null? x state)
  (if (null? x)
      #t
      (if (null? (car x) )
          #t
          #f)))


;evaluates number?
;Performs the number? function
;Arguments:
; x : list of arguments to the function
; state : the current state of the stack
; Returns : #t or #f
(define (eval_num? x state) (if (number? x) #t #f))


;evaluates quote
;Performs the let function
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns : #t or #f.
(define (eval_quote x state)
  (if (null? x)
      '()
      (if (pair? x)
          (if (equal? (length x) 1)
          (car x)
          (error "Eval_quote: Incorrect Length" x " :state: " state))
          (error "Eval_quote: Pair instead of list" x " : state : " state))))


;evaluates the if function
;Performs if function
;Arguments:
; x : the list of arguments to the function
; state : the current state of the stack
;Returns : asks execute to evaluate the true body if true or the false body if false
(define (eval_if x state)
  (if (list? x)
    (if (list? (first x))
      (let ([a (execute(first x) state)])
      (if(equal? a #t)
        (execute (second x) state)
        (execute (third x) state)))
      (error "everything is on fire in eval_if."))
    (error "everything is on fire in eval_if.")))


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
(define (eval_lambda lamb param bound state)
  (print (list " : lamb : " lamb " : param : " param " : state : " state))
  (let ([arg (cadr lamb)][body (caddr lamb)])
    (if (null? arg)
        (execute (cons body param) (addToState bound state))
        (if (null? param)
            (list 'lambda arg (list 'let bound body)) ;boundTOList: convert from pair to list
            (let ([addition (cons (car arg) (cons (car param) '()))] [argContinuation (cdr arg)] [paramContinuation (cdr param)])
              (eval_lambda (list 'lambda argContinuation body) paramContinuation (cons addition (cons bound '())) state))))
    )
  )
                     
; Computes the resulting state of the stack where a pair to be added onto the current state of the stack
; Takes p, the pair (x 1) to be added, and state, the current state of the stack
; Returns the new stack with the state added
(define (producePristinePair p state)
  (if (and (pair? p) (not (null? p)))
  (cons (car p)  (car (cdr p)))
  (error "You have caused a calamity: producePristinePair on " p " state : " state)))

;Append all the key value pairs in a list (x 3) into the state ( (x . 3) (y . 5) ) -> state
;Calls producePristinePair to perform the minimal processing necessary
;Does not evaluate anything
(define (addToState vars state)
  (if (and (pair? vars) (not (null? vars)))
      (let ([addition (producePristinePair (car vars) state)] [rest (cdr vars)])
           (if (null? rest)
               (cons addition state)
               (cons addition (addToState rest state))))
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
            (cons addition state)
               (cons addition (evalToState rest state))))
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

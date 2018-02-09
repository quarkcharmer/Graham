
;;********PAUL GRAHAM - ANSI COMMON LISP***********************
;;********CHAPTER 2********************************************
;;The evaluation model for functions is very simple. When the
;;valuator encounters a form (f a1 a2...) then it presumes that
;; the symbol named f is one of the following:
;;1. A "special operator" (easily checked against a fixed list)
;;2. A "macro operator" (must have been defined previously)
;;3. "The name of a function" (default), which may either be a
;;    symbol, or a sub-form beginning with the symbol lambda.
;;
;;    If f is the name of a function, then the arguments a1,
;;    ..a2.., are evaluated in left-to-right order, and the
;;    function is found and invoked with those values supplied
;;    as parameters



;;test example
(defun negate (X)
  "Negate the value of X."  ; This is a documentation string.
  (- X))

;;what you can't do in C:
(defun addn (n)
  #'(lambda (x) (+ x n)))

;;adding a newline
(write-line "Hello-World")


;;easy examples
(write (* 7 (+ 8 9)))
(write-line "   ")
(write (/ (* 7.0 (+ 8.0 9.0)) 10.0))

;;defining a function/procedures
(defun C-to-F (X)
  "convert celsius to fahrenheit"
  (+ 32.0 (* 9 (/ X  5))))
(write-line "   ")
(write (negate 8))
(write-line "   ")
(write-line " converting 100.0 C to F")
(write (C-to-F 100.0))
(defun F-to-C (X)
  "convert fahrenheit to celsius"
  (* 5 (/ (- X 32) 9))
  )
(write-line "   ");;before  i learnt ~%
(write-line " converting 212.0 F to C")
(write (F-to-C 212.0))

;;defining a macro...what's the difference????
(defmacro SetTo10 (NUM)
  (setq NUM 10)(print NUM))

;;list function 'car'
(defun my-last (x)
  (car ( reverse x)))

;;recursion
(defun my-member ( obj lst)
  (if (null lst)
      nil
      (if (eql obj (car lst))
	  lst
	  (my-member obj (cdr lst))
	  )
      )
  )
(write-line "   ")

;;output
(format t "~A plus ~A equals ~A.~%" 2 3 (+ 2 3))
(format t "~A is ~A." "my name" "jon")

;;input...this function returns the entered text
(defun askem (string)
  (format t "~A~%" string)
  (read)
  )

;;variables
(let ((x 3) (y 5))
  (* x y)
  )

(defun ask-a-number ()
  (format t "enter a number: ~%")
  (let ((val (read)))
  (if (numberp val)
      val
      (ask-a-number))))

(setf x (list 'a 'b 'c))
(setf (car x) 'z)
(setf a 'b c 'd)

;;functional programming ....everything returns a value
(setf lst '(c a r a t))
(remove 'a lst)

;; and it avoids doing the following kind of thing...no need !!!
;; the slogan is: AVOID SIDE EFFECTS...result,result,result...
;; until you arrive where you want from where you started
;; without changing anything that didn't need changing/creating
(setf L (remove 'a lst))

;;iteration ...returns done
(defun show-squares (start end)
  (do ( (i start (+ i 1)))
	((> i end) 'done)
    (format t "~A squared is: ~A ~%" i (* i i))))

;;recursion version...returns done
(defun show-squares-R (i end)
  (if (> i end)
      'done
      (progn
	(format t "~A squared is: ~A ~%" i (* i i))
	(show-squares-R (+ i 1) end))))


(defun my-factorial ( n)
  (if (eql n 0)
      1
      (* n (my-factorial (- n 1)))))


;;functions as objects
(function show-squares-R)
;;;#'show-squares-R

(apply #'show-squares-R '(0  4))
;; any number of args but last must be a list
(apply #'+ 1 2 '(3 4 5))
;;or without a list use:
(funcall #'+ 1 2 3 4 5 6)

;; using lambda to mark a function..same result as function and #'
(lambda ( x y z)
  (+ x y z))
;;apply an anonymous function
((lambda (x) (+ x 100)) 1)
;;and this applies to a  long seq 
(funcall #'(lambda ( a b c d e) (+ a b c d e)) 1 2 3 4 5)

;; in list values have type not variables
;; values are in a hierarchy of types
(typep 27 'integer)
(typep 27 'real)
(typep 27 'atom)
;;etc up to: t


;;from the chapter 1 exercises
;;Ex 3
(defun my-fourth (X)
  (car (cdr ( cdr ( cdr X)))))
;;Ex 4
(defun the-greater (x y)
  (if (eql x y)
      'neither
      ( if (> x y) x y)))
;;Ex 5a
(defun enigma (x)
  (and (not (null x))
       (or (null (car x))
	   (enigma (cdr x)))))
;;Ex 5b
(defun mystery ( x y)
  (if (null y)
      nil
      (if (eql (car y) x)
	  0
	  (let ((z (mystery x (cdr y))))
	    (and z (+ z 1))))))

;;Ex 6a
(car (car (cdr '(a (b c) d))))
;;Ex 6b
(or 13 (/ 1 0))
;;Ex 6c
(apply #'list 1 nil)
;;Ex 7
(defun check-list (x)
  (if (null x)
      nil
      (if (listp (car x))
	  t
	  (check-list (cdr x)))))

;;Ex 8a iterative version
(defun the-dots (x)
  (do ((i 1 (+ i 1)))
      ((> i x) 'done)
    (format t "~a" ".")))
;; and a (weird) recursive versio
(defun the-dots-r (x)
  (if (eql x 0)
      nil
      (if (not (format t "~a" "."))
	  (the-dots-r (- x 1)))))

;;Ex 8b recursive
(defun find-a-r (x)
  (if (null x)
      nil
      (if (eql 'a (car x))
	  t
	  (find-a-r (cdr x)))))
;; iterative version
(defun find-a (x)
  (if (null x)
      nil
      (loop for w in x
	  do  (if (eql w 'a)
		  (return t)))))
;; extra
(defun find-any (lst x)
  (if (null lst)
      nil
      (loop for w in lst
	 do (if (eql w x)
		(return t)))))


		 
;; a version using dolist
(defun find-a-2 (x)
  (if (null x)
      nil
      (dolist (i x)
	(if (eql i 'a)
	    (return t)))))
	  
	        
;;Ex 9a
(defun summit (x)
  (setf w (remove nil x))
  (apply #'+ w))

(defun summit2 (lst)
  (if (null lst)
      nil
      (let ((x (car lst)))
	(if (null x)
	    (summit2 (cdr lst))
	    (+ x (summit (cdr lst)))))))

;;********END OF CHAPTER 2**************************************

;;**************************************************************
;;example from hyperspace on lookup of "lexical closure"
;; creates a list of two functions
(defun two-funs (x)
  (list (function (lambda () x))
	(function (lambda (y) (setq x y)))))
;;instantiate  it  with  an argument...and a name
(setq funs (two-funs 5))
;;use the first one:
(funcall (car funs))
;;use the 2nd one:
(funcalL (cadr funs) 6)
;;**********************************************************

;;********CHAPTER 3: LISTS**********************************

;;CONS******************************************************
;;lists are pairs of Cons..chains of pairs of Cons
(setf x (cons 'a nil))
(car x)
(cdr x)

;;nested list
(setf z (list 'a (list 'b 'c) 'd 'e))

;;a list is a cons
(consp '(a b c))
;;everything else is a list  or an atom:
(atom 'a)
;;nil is both:
(atom nil)
(listp nil)

;;EQUALITY**************************************************
;; eql requires (memory) identity of objects, so not:
(eql ( cons 'a nil) (cons 'a nil))
;; i.e. singular object

;; equal allows "print equivalence"
(equal (cons 'a nil) (cons 'a nil))

;;WHY LISP HAS NO POINTERS**********************************
;;variables are pointers to their value
(setf x '(a b c))
(setf y x)
;; so x and y point to same list

;;BUILDING LISTS********************************************
;;copying a list:
(setf x '( s t))
(setf y (copy-list x))


;;concatenating lists:
(append '(a b c) '(d e f) '(g h))


;;AN EXAMPLE: COMPRESSION***********************************
;;compression: run-length encoding
(defun compress (x)
  (if (consp x)
      (compr (car x) 1 (cdr x))
      x))
(defun compr (elem n lst)
  (if (null lst)
      (list (n-elems elem n))
      (let ((next (car lst)))
      (if (eql next elem)
	  (compr elem (+ n 1) (cdr lst))
	  (cons (n-elems elem n)
		(compr next 1 (cdr lst)))))))
(defun n-elems (elem n)
  (if (> n 1)
      (list n elem)
      elem))
(compress
 '( 1 1 1 3 3 0 2 2 2 2 1 1))

(defun uncompress (lst) 
  (if (null lst)
      nil
      (let ((elem (car lst))
	    (rest (uncompress (cdr lst))))
	(if (consp elem)
            (append (apply #'list-of elem)
		    rest)
		    (cons elem rest)))))

(defun list-of (n elem)
  (if (zerop n)
      nil
      (cons elem (list-of (- n 1) elem))))
      
;;*********exploring ideas on permutation*******************
;;for each element in n elements construct (n-1)!  lists which
;;begin with that element and are the permutations of the
;;remaining n-1 elements.

;;HOW TO DO THIS IN THE (PROVABLY) MOST EFFICIENT/QUICKEST WAY

(defun missing-out (x lst)
  (dolist (i lst)
         (if (eql i x)
          (continue)
          (format t "~a" i))))

(defun missing-cycle (x)
  ;;prints the list remaining when each elem of original list
  ;;is missed out in turn
    (dolist (i x)
      (progn
        (missing-out i x)
        (format t "~%"))))


;;**********************************************************


;;ACCESS FUNCTIONS******************************************
;;5th member starting at 0th
(nth 5 '( 1 1 1 3 3 0 2 2 2 2 1 1))

;;the cdr starting from 5th:
(nthcdr 5 '( 1 1 1 3 3 0 2 2 2 2 1 1))

;;or building a simple version:
(defun our-nthcdr (n lst)
  (if (zerop n)
      lst
      (our-nthcdr (- n 1) (cdr lst))))

(last '( a b c d e)) ;;gives the last cons
(car (last '(a b c d e))) ;;gives the last element

;; first ...tenth give those elements of a list, starting a 1:
(fifth '( 1 1 1 3 3 0 2 2 2 2 1 1))


;;MAPPING FUNCTIONS*****************************************
;;applying a function to all elements of a list:
(mapcar #'(lambda(x) (+ x 10)) '( 1 2 3 4 5))
;;and a weird example:
(mapcar #'list '(a b c) '(d e f g))
;;which gives iteration over multiple lists:
(mapcar #'(lambda( x y) (+ x y)) '(1 2 3) '(4 5 6))
;;and a variation which runs thru cdrs of the list:
(maplist #'(lambda(x) x) '(a b c))
(maplist #'(lambda(x) (car x)) '(a b c d))
;;see later for: mapc and mapcan
(defun addn(n)
  #'(lambda (x)
      (+ x n)))
(funcall (addn 10) 2)

;; functions are first-class objects
(setq f10 (addn 10))
(funcall f10 3)

;;or
(defmacro f5 (n) (+ 5 n))
(f5 4)

;;TREES*****************************************************
;;tree structure of cons; see diag p 41
(car '(a (b c ) d))
(car ( cdr '(a (b c ) d)))
(cdr (cdr '(a (b c ) d)))

;;copying a tree: 
(setf w (copy-tree '(a (b c ) d)))
;;n.b. copy not same obj:
(setf x '(a (b c ) d))
(setf y (copy-tree x))
(eq x y) ;; nil
(equal x y) ; T

;;could define this as:
(defun our-copy-tree (tr)
  (if (atom tr)
      tr
      (cons (our-copy-tree (car tr)
			   (our-copy-tree (cdr tr)))))
(equal (our-copy-tree '(a (b c) d))
       (copy-tree '(a (b c) d))) ;; T

(our-copy-tree '(a (b c)d))

;;working on trees: a sexp is a form of tree.
;; can't use SUBSTITUTE as it applies only to sequences:
(substitute 'z 'a '(a b c d))
;;have to use SUBST which works on trees:
(subst 'z 'a '(and (integerp a) (zerop (mod a 2))))
(subst '(* s 4) 'a '(and (integerp a) (zerop (mod a 2))))
;;suggests how LISP can create code dynamically, by setting
;; a variable to the returned list and using #' with it

;;can define a version of SUBST using the usual
;; DOUBLE RECURSION ...car and cdr... for tree functions:
(defun our-subst (new old tree)
  (if (eql old tree) ;;shouldn't this be EQUAL
      new
      (if (atom tree)
	  tree
	  (cons (our-subst new old (car tree))
		(our-subst new old (cdr tree))))))
(our-subst 'z 'a '(and (integerp a ) (zerop (mod a 2))))

;;Q-define a function to extract leaves of a tree
;; is it an atom or a list...????


;;SETS******************************************************
(member 'b '(a b c d)) ;;returns the list from 'b
;; a cons = T
;;using a KEYWORD arg to change from eql
(member 'b '(a b c d) :test #'equal)
(member 'a '((c d) (e f) (a b)) :key #'car)
;;in this last all have to be lists so car works
;; to find a member with car = 2 use both keywords, any order
(member 2 '( ( 1 2) () (2 3)) :test #'equal :key #'car)

;; to select with a predicate  not a function use:
(member-if #'oddp '(  2 3 4))

;; a simple version of member-if might be:
(defun our-member-if (fn lst)
  (and (consp lst) ;; bit subtle using an "and" 
       (if (funcall fn (car lst))
	   lst
	   (our-member-if fn (cdr lst)))))

;; adjoin is an exclusive version of cons:
(adjoin 'b '(a b c)) ;;returns ( a b c) as b already a member
(adjoin 'b '(a c)) ;;returns ( b a c)

;;set operations:
(union '(a b c d) '(c f g))
(intersection '(a b c d) '(c f g))
(set-difference '(a b c d) '(c f g))
;;this last returns what's in first when second is removed

;;SEQUENCES*************************************************
(length '(a b c))
(subseq '(a b c d e f g) 3 6)
(reverse '(a b c))
;; a simple palindrome function...even only:
(defun mirror? (lst)
  (let ((len (length lst)))
  (and (evenp len)
       (let ((mid (/ len 2)))
	 (equal (subseq lst 0 mid)
		(reverse (subseq lst mid )))))))

(mirror?  '(a b c c b a))

;; sort is a higher order function, taking a function
;; as one of the arguments:
(sort '( 5 2 7 1 9 3) #'>) ;; nb changes list ! so:
(sort (copy-list '(5 2 7 1 9 3)) #'>)
(sort '(a z b e c)  #'string< :key #'symbol-name)

(defun nth-most (n lst)
  (nth (- n 1)  ;; n is indexed from 0
       (sort ( copy-list lst) #'>)))

(nth-most 3 '( 9 1 6 5 8 0))  ;;returns 6

(defun pair-sum (p)(+ (first p) (second p)))
(sort (list '(9 2) '( 6 7)) #'< :key 'pair-sum)

;; EVERY and SOME:
(every #'evenp '( 1 2 3 4 5 6)) ;; nil as not all are even
(some #'oddp '( 1 2 3 4 5 6))  ;; T as some are odd

;;STACKS****************************************************
;; can define push:
(setf lst (cons 'a '(b c)))
(push 'z lst)  ;; ( z a (b c))
;; and pop:
(let ((x (car lst)))
  (setf lst (cdr lst)) x)  ;; z
(setf x '(b))
(push 'a x)
(setf y x)
(pop x)  ;;a but y=(a b)
;;why can't you do this->
;; (push 'z '(a b c))
;;or even this->
;; (push 'z (list 'a 'b))

;;using push to define REVERSE->
(defun our-reverse (lst)
  (let ((rev nil))
    (dolist (elem lst)
      (push elem rev))
    rev))

;;the PUSHNEW macro uses ADJOIN: n.b. only adds if not already
(let ((x '(a b)))
      (pushnew 'z x) 
      (pushnew 'y x)
      x)

;;DOTTED LISTS**********************************************
;;proper list  is nil or a cdr which is proper
(defun proper-list? (lst)
  (or (null lst)
      (and (consp lst)
	   (proper-list? (cdr lst)))))
;;can use CONS  to build any kind of structure->
(setf pair (cons 'a 'b)) ;;and this gives a DOTTED LIST(A . B)
;; i,e, not really a list, just a 2-part structure. The cons
;; for this has both "cells" pointing at a value, not the usual
;; second pointing at another cons/nil
;; compare->
(cons 'a '(b)) ;;which gives a PROPER LIST (A B) as usual

;;ASSOC LISTS***********************************************
;;using the CONS structure to represent a mapping->
(setf trans '((+ . add) (- . subtract)
	      (* . times) (/ . divide)))
;;use ASSOC  to find the map from a key->
(assoc '/ trans)   ;;returns (/ . DIVIDE)
;; assoc returns the FIRST CONS in the alist whose 
;; CAR satisfies the test; in the example the 'default test'
;; is equal to the key symbol /

;;a simple version of ASSOC could be->
(defun our-assoc (key trans)
  (and (consp trans)
       (let ((pair (car trans)))
	 (if (eql key (car pair))
	     pair
	     (our-assoc key (cdr trans))))))

(our-assoc '* trans)


;;EXAMPLE: SHORTEST PATH************************************
;;******** see separate file: shortest-path.lisp
;;**********************************************************

;;GARBAGE***************************************************
;; slower traversing lis versus array acccess
;; automatic memory management
;; memory allocation from heap "consing"
;; garbage collection
;; too much consing? use DESTRUCTIVE FUNCTIONS, see later
;;**********************************************************
;;********EXERCISES on Chapter 3 ***************************
;;
;;ex2 new union preserving order->
(defun new-union ( x y)
  (if (or (null x) (null y))
      nil
      (append (list (car x) (car y) )
	      (new-union (cdr x) (cdr y)))));; bootiful

;;ex3 number times each item appears in a list->
(defun my_occurrences ( lst )
  (setq occ_lst ()) 
  (setq lst_len (length lst))
  (dolist  (elem lst)
    (if (not (null (assoc elem occ_lst)))
	(continue);;avoid repetition of cases
	(progn
	  (setq n (- lst_len (length (remove elem lst))))
	  (setq occ_lst (append occ_lst
				(list  (cons elem n)))))))
  occ_lst)
;; this took many hours - half to find the idea
;; mostly being fixed on one approach and slowly realising
;; it generated too many problems: viz iterating across the
;; list and counting as you go. Chuffed at finding this
;; solution

(sort (my_occurrences '(a b c a z s z f g a b)) #'string< :key #'car) ;; puts the analysis in alphabetical order
  


;;ex4: because (a) is a member of '((a) (b)) not 'a
;;ex5 adds its position to a list of numbers
;;(i) by iteration
(defun pos+ (lst);;*** must be a better way !!!********
  (progn
    (setq n 1)
    (setq new_lst ())
    (dolist (elem lst)
      (progn
	(setq new_lst (append new_lst (list (+ elem n))))
	(setq n (1+ n))
	))
    new_lst))

;;(ii) by recursion->
(defun pos+rec (lst)
  (cond
    ((null lst) nil)
    ( t (cons (1+ (car lst))  (pos+rec (cdr lst))))))

;; slightly different exercise of finding position of
;; an item in a list...i found with Google
(defun position-in-list (letter liste)
   (cond
      ((atom liste) nil)
      ((equal letter (car liste)) 0)
      ((position-in-list letter (cdr liste)) (+ 1
	     (position-in-list letter (cdr liste))))))

;;*****************CHAPTER 4********************************
;;********ARRAYS and VECTORS************
(setf arr (make-array '(2 2) :initial-element 1))
;; access
(aref arr 0 1)
(setf (aref arr 0 1) 2)
;; to print an array, set the global var:->
(setf *print-array* t)
;; it will print using the #n syntax:
arr --> #2((1 2) (1 1))

;;pseudo-vector: single row array:->
(setf p-vec (make-array 5 :initial-element 'w))

;; but better is a vector:->
(setf vec (vector 1 'e 3 'x))
;; and access it using:->
(svref vec 2)

;;*********STRINGS & CHARACTERS**********
;; Strings are vectors of characters: constant string in double Quotes
;; and character c -> #\c
(char-code #\a) ;; returns number for 'a
(code-char 97) ;; returns #\
;;character relations->
;; char<,char=,char>,char>=,char<= and char/=
(sort "elbow" #'char<)
;;strings are vectors so->
(aref "name" 2) ;;returns #\m...n.b index from 0
;;quicker is->
(char "wobbly" 3) ;;returns #\b
;; and can use setf or aref to replace a char in a string
(let ((str (copy-seq "man")))
  (setf (char str 2) #\d)
  str)
;; and to compare strings->
(string-equal "fred" "FRED") ;; case independent

;;to build strings, output to nil->
(format nil "~A~A" "jock" "ey")
;;or ->
(concatenate 'string "hello " "world")

;;*********SEQUENCES*********************
(setf vec2 (vector 1 2 3 4))






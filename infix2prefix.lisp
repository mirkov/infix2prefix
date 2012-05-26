;; Mirko Vukovic
;; Time-stamp: <2012-05-25 13:06:34 infix2prefix.lisp>
;; 
;; Copyright 2012 Mirko Vukovic
;; Distributed under the terms of the GNU General Public License
;; 
;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.
;; 
;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.
;; 
;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

(in-package #:infix2prefix)




(defun ncollect-pairs (terms ops op-selector)
  "Parse successive pairs of the TERMS list, and in parallel
successive operators in the OPS list and build a resulting TERMS1 and
OPS1 list.

When the operator satisfies the op-selector function, collect the
current pair and operator as a (op arg arg) list into the TERMS1 list

Otherwise, collect the car of the pair and the operator into the
TERMS1 and OPS1 lists.

This is a destructive function which modifies the arguments

For details see the unit-test"
  (let ((terms1) (ops1) (flag))
    (do ((term1 (pop terms) (pop terms))
	 (term2 (car terms) (car terms))
	 (op (pop ops) (pop ops)))
	((not term2) (push term1 terms1))
      (if (funcall op-selector op)
	  (progn
	    (push (list op term1 (pop terms)) terms)
	    (setf flag t))
	  (progn
	    (push term1 terms1)
	    (push op ops1))))
    (values (nreverse terms1) (nreverse ops1) flag)))


(defmacro assert-parse (arg-expected op-expected flag-expected operation)
  (with-gensyms (arg-res op-res flag-res)
    `(multiple-value-bind (,arg-res ,op-res ,flag-res)
	 ,operation
	 (assert-equal ,arg-expected ,arg-res)
	 (assert-equal ,op-expected ,op-res)
	 (assert-equal ,flag-expected ,flag-res))))

(define-test ncollect-pairs
  ;; pass-through test
  (assert-parse '(a b c d) '(+ + -) nil
		(ncollect-pairs  (list 'a 'b 'c 'd) (list '+ '+ '-)
				 (lambda (op)
				   nil)))
  ;; single argument test
  (assert-parse '(a) nil nil
		(ncollect-pairs (list 'a) nil (lambda (op)
						nil)))
  ;; two arguments
  (assert-parse '(a b) '(+) nil
		(ncollect-pairs (list 'a 'b) (list '+)
				(lambda (op)
				  (equal '* op))))
  (assert-parse '((* a b)) 'nil t
		(ncollect-pairs (list 'a 'b) (list '*)
				(lambda (op)
				  (equal '* op))))
  ;; more complex combinations
  (assert-parse '(a (* b c) d) '(+ -) t
		(ncollect-pairs (list 'a 'b 'c 'd) (list '+ '* '-)
				(lambda (op)
				  (equal '* op))))

  (assert-parse '(a b (* c d)) '(+ -) t
		(ncollect-pairs (list 'a 'b 'c 'd) (list '+ '- '*)
		 (lambda (op)
		   (equal '* op))))

  (assert-parse '((* a b) (* c d)) '(-) t
		(ncollect-pairs (list 'a 'b 'c 'd) (list '* '- '*)
				(lambda (op)
				  (equal '* op)))))


(defun ncollect-all-pairs (terms ops op-selector)
  "Collect all pairs in TERMS that satisfy OP-SELECTOR function when
  applied to OPS

This function calls repeatedly ncollect-pairs, until no further pairs
can be built.

The purpose of this function is to parse terms like 'a*b/c' into (/ (*
a b) c)

This is a generic function.  More specific functions are built on top
of it
"
  (loop
     (multiple-value-bind (terms1 ops1 flag)
	 (ncollect-pairs terms ops op-selector)
       (when (not flag)
	 (return-from ncollect-all-pairs
	   (values terms1 ops1)))
       (setf terms terms1
	     ops ops1))))


(defmacro assert-parse1 (arg-expected op-expected operation)
  (with-gensyms (arg-res op-res)
    `(multiple-value-bind (,arg-res ,op-res)
	 ,operation
	 (assert-equal ,arg-expected ,arg-res)
	 (assert-equal ,op-expected ,op-res))))

(defun ncollect-*/-pairs (terms ops)
  "Collect all pairs in TERMS that have * or / between them"
  (ncollect-all-pairs terms ops
		     (lambda (op)
		       (or (equal op '*)
			   (equal op '/)))))

(define-test collect-*/-pairs
  (assert-parse1 '((* a b) (* c d)) '(-)
		 (ncollect-*/-pairs (list 'a 'b 'c 'd) (list '* '- '*)))
  (assert-parse1 '((* a b) (/ c d)) '(-)
		 (ncollect-*/-pairs (list 'a 'b 'c 'd) (list '* '- '/)))
  (assert-parse1 '((/ (* (* a b) c) d)) nil
		 (ncollect-*/-pairs (list 'a 'b 'c 'd) (list '* '* '/))))

(defun ncollect-+--pairs (terms ops)
  (ncollect-all-pairs terms ops
		     (lambda (op)
		       (or (equal op '+)
			   (equal op '-)))))

(define-test ncollect-+--pairs
  (assert-parse1 '((- (+ (+ a b) c) d)) nil
		 (ncollect-+--pairs (list 'a 'b 'c 'd) (list '+ '+ '-))))

(defun parse-algebraic-expression (terms ops)
  "Parse algebraic expression defined by the two lists TERM and OPS
 and return a single list containing the expression in infix notation.

TERMS contains the terms of the expression and OPS the operators

OPS length must be one less than the TERMS length.

TERMS and OPS may be destructively modified

For examples, see the unit tests
"
  (assert (= (length terms)
	     (1+ (length ops))) ()
	     "TERMS and OPS lengths should differ by one")
  (if (= 1 (length terms))
      (assert (null ops) ()
	      "OPS: ~a must be null when only one term is present" ops)
      (assert (find-if-not (lambda (op)
			     (not (find op *ops-bag*)))
			   ops) ()
			   "Found invalide operator in OPS: ~a" ops))
  (multiple-value-bind (terms1 ops1)
      (ncollect-*/-pairs terms ops)
    (multiple-value-bind (terms2 ops2)
	(ncollect-+--pairs terms1 ops1)
      (assert (null ops2) ()
	      "OPS1: ~a is expected to be nil" ops2)
      (car terms2))))

(define-test parse-algebraic-expression
  (assert-equal '(- (+ (+ a b) c) d)
		(parse-algebraic-expression (list 'a 'b 'c 'd) (list '+ '+ '-)))
  (assert-equal '(- (+ a (* b c)) d)
		(parse-algebraic-expression (list 'a 'b 'c 'd) (list '+ '* '-)))
  (assert-equal '(- (+ a b) (* c d))
		(parse-algebraic-expression (list 'a 'b 'c 'd) (list '+ '- '*)))
  (assert-equal 'a (parse-algebraic-expression (list 'a) nil)))
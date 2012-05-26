;; Mirko Vukovic
;; Time-stamp: <2012-05-25 21:52:10 parsing-functions.lisp>
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

(in-package :infix2prefix)

(defun split-infix (string &optional (bag *ops-bag*))
  "Split sequence at arithmetic operators and return the list of
terms.  The second value returned is a list of operators themselves

STRING -- contains arithmetic expression
BAG -- a list of operator symbols"
  (let ((delimiters nil))
    (values
     (split-sequence-if (lambda (char)
			  (awhen (find char *forbidden-operators* :test #'string=)
			    (error "char: ~a is not allowed" char))
			  (awhen (find char bag :test #'string=)
			    (push it delimiters)
			    it))
			string)
     (nreverse delimiters))))

(define-test split-infix
  (multiple-value-bind (args operators)
      (split-infix "1+2/7")
    (assert-equal '("1" "2" "7") args)
    (assert-equal '(+ /) operators))
  (multiple-value-bind (args operators)
      (split-infix "32")
    (assert-equal '("32") args)
    (assert-equal nil operators))
  #|(split-infix "2^9")|#)




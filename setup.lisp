;; Mirko Vukovic
;; Time-stamp: <2012-05-25 21:52:17 setup.lisp>
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

(defvar *ops-bag* '(+ - * /)
  "List of valid operators")

(defvar *forbidden-operators* '(\( \) ^)
  "List of forbidden operators.  These will throw an error")


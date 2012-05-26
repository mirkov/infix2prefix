;; Mirko Vukovic
;; Time-stamp: <2012-05-25 21:51:44 infix2prefix.asd>
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

;;;; infix2prefix.asd

(asdf:defsystem #:infix2prefix
  :serial t
  :depends-on (#:lisp-unit
	       #:alexandria
	       #:split-sequence
	       #:anaphora)
  :components ((:file "infix2prefix-package-def")
	       ;; special variables
	       (:file "setup")
	       ;; conversion of infix 2 prefix
               (:file "infix2prefix")
	       ;; parsing of infix strings into a form usable by infix2prefix
	       (:file "parsing-functions")))


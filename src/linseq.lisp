#|                                           -*- mode: lisp; coding: utf-8 -*-
  Deterministic Arts -- Pipes / Streams / Series
  Copyright (c) 2011 Dirk Esser

  Permission is hereby granted, free of charge, to any person obtaining a copy
  of this software and associated documentation files (the "Software"), to deal
  in the Software without restriction, including without limitation the rights
  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
  copies of the Software, and to permit persons to whom the Software is
  furnished to do so, subject to the following conditions:

  The above copyright notice and this permission notice shall be included in
  all copies or substantial portions of the Software.

  THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
  THE SOFTWARE.
|#

(in-package "DARTS.LIB.PIPES")

(defgeneric head (sequence)
  (:documentation "Returns the first element of the given sequence. Signals
a condition, if the sequence is empty. Think of this function as a generalization
of car."))

(defgeneric tail (sequence)
  (:documentation "Returns a sequence containing all elements of the given
sequence except the first element. Signals a condition, if the sequence is 
empty. Think of this function as a generalization of cdr."))

(defgeneric emptyp (sequence)
  (:documentation "Returns true, if the given sequence is empty, and false
otherwise. Think of this function as a generalization of null."))


(defmethod head ((cons cons)) (car cons))
(defmethod tail ((cons cons)) (cdr cons))
(defmethod emptyp ((cons cons)) nil)
(defmethod emptyp ((null null)) t)

(defmethod head ((vector vector))
  (if (zerop (length vector))
	  (error "attempt to take head of empty sequence")
	  (aref vector 0)))

(defmethod tail ((vector vector))
  (let ((length (length vector)))
	(if (zerop length)
		(error "attempt to take tail of empty sequence")
		(make-array (list (- length 1))
		  :element-type (array-element-type vector)
		  :displaced-to vector 
		  :displaced-index-offset 1))))

(defmethod emptyp ((vector vector))
  (zerop (length vector)))


(defmethod head ((pipe pipe))
  (pipe-head pipe))

(defmethod tail ((pipe pipe))
  (pipe-tail pipe))

(defmethod emptyp ((pipe empty-pipe))
  t)

(defmethod emptyp ((pipe pair-pipe))
  nil)


(defun fold-left (function seed sequence)
  (labels
	  ((fold (sequence value)
		 (if (emptyp sequence) value
			 (fold (tail sequence) (funcall function value (head sequence))))))
	(fold sequence seed)))

(defun fold-right (function seed sequence)
  (labels 
	  ((fold (sequence stack)
		 (if (emptyp sequence)
			 (loop
				:for value := seed :then (funcall function head value)
				:for head :in stack
				:finally (return value))
			 (fold (tail sequence) (cons (head sequence) stack)))))
	(fold sequence '())))

(defun maphead (function sequence)
  (loop
	 :for link := sequence :then (tail link)
	 :until (emptyp link)
	 :do (funcall function (head link))
	 :finally (return (values))))


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

(defstruct (pipe
			 (:predicate pipep))
  "Basic lazy evaluation device. A pipe is a data structure similar
to a standard Lisp list. A pipe is either empty, or an object composed
of a head value and a tail value, the latter being lazily evaluated on
demand only. Note, that unlike regular conses, the tail values of a 
pipe (once forced into existence) must always be proper pipes 
themselves.

You can test, whether a pipe is empty by applying the empty-pipe-p
predicate. Reading a pipe's head and tail values may be done using the
pipe-head and pipe-tail accessors. Reading the tail of a pipe, which
has not yet been computed will force the evaluation of the tail 
expression. This happens at most once for each pipe.")

(defstruct (empty-pipe
			 (:include pipe)
			 (:predicate empty-pipe-p)
			 (:constructor %make-empty-pipe)))

(defstruct (pair-pipe
			 (:include pipe)
			 (:constructor %make-pair-pipe (head %tail &optional forced)))
  (head nil :read-only t)
  (forced nil)
  (%tail nil))


(defparameter +empty-pipe+ (%make-empty-pipe)
  "The canonical empty pipe object. This value is the only representation
of empty pipes defined by this package.")


(defmethod print-object ((ob pair-pipe) stream)
  (print-unreadable-object (ob stream :type t)
	(format stream "~S~A"
			(pair-pipe-head ob)
			(if (pair-pipe-forced ob) " forced" ""))))


(defmacro pipe (head &rest tails)
  "pipe HEAD1 [HEAD2 ... HEADN] [TAIL] => PIPE

Constructs a new pipe, whose first N head values are HEAD1 ... HEADN,
followed by the pipe returned by TAIL. Note, that all HEADk values are
evaluated directly. The last argument to this macro, however, is 
actually evaluated lazily, on demand."
  (cond ((null tails) `(%make-pair-pipe ,head +empty-pipe+ t))
		((null (cdr tails)) `(%make-pair-pipe ,head #'(lambda () ,(car tails))))
		(t `(%make-pair-pipe ,head (pipe ,@tails) t))))


(defun make-pipe (head &optional (tail +empty-pipe+))
  "make-pipe HEAD &optional TAIL => PIPE

Constructs a new pipe with the given HEAD value and TAIL. The TAIL
may either be another pipe, or a function designator (symbol, function),
for a function which can be funcalled without arguments."
  (check-type tail (or function symbol pipe))
  (%make-pair-pipe head tail (pipep tail)))


(defun empty-pipe ()
  "empty-pipe => PIPE

Returns an empty pipe, i.e., a pipe, for which empty-pipe-p will
always return true, and for which pipe-head and pipe-tail will always
fail."
  +empty-pipe+)


(defun pipe-head (pipe)
  "pipe-head PIPE => VALUE

Obtains the given pipe's head value. If PIPE is not a non-empty pipe
object, a condition of type simple-error is signalled."
  (if (pair-pipe-p pipe) 
	  (pair-pipe-head pipe)
	  (error "value ~S is not a non-empty pipe" pipe)))


(defun pipe-tail (pipe)
  "pipe-tail PIPE => VALUE

Obtains the given pipe's tail pipe. If PIPE's tail has not yet been
computed, this method will force the computation now. If PIPE is not
a non-empty pipe object, this method signals a condition of type
simple-error."
  (if (not (pair-pipe-p pipe))
	  (error "value ~S is not a non-empty pipe" pipe)
	  (let ((tail (pair-pipe-%tail pipe))
			(forced (pair-pipe-forced pipe)))
		(if forced tail
			(let ((value (funcall tail)))
			  (setf (pair-pipe-forced pipe) t)
			  (setf (pair-pipe-%tail pipe) value))))))


(defun map-pipe (function pipe &rest more)
  "map-pipe FUNCTION PIPE &rest MORE => NEW-PIPE

This function returns a new pipe, whose head values are derived from
PIPE by applying FUNCTION to each of them. The FUNCTION must accept
as many input arguments as pipes are supplied via PIPE and MORE. The
resulting NEW-PIPE will be exhausted as soon as the shortest input
pipe is."
  (labels ((map-1 (pipe)
			 (if (empty-pipe-p pipe)
				 +empty-pipe+
				 (pipe (funcall function (pipe-head pipe))
					   (map-1 (pipe-tail pipe)))))
		   (map-2 (pipe1 pipe2)
			 (cond ((empty-pipe-p pipe1) +empty-pipe+)
				   ((empty-pipe-p pipe2) +empty-pipe+)
				   (t (pipe (funcall function (pipe-head pipe1) (pipe-head pipe2))
							(map-2 (pipe-tail pipe1) (pipe-tail pipe2))))))
		   (heads+tails (pipes heads tails)
			 (if (null pipes)
				 (values (nreverse heads) (nreverse tails))
				 (let ((p (car pipes)))
				   (if (empty-pipe-p p) 
					   (values nil nil)
					   (heads+tails (cdr pipes)
									(cons (pipe-head p) heads)
									(cons (pipe-tail p) tails))))))
		   (map-n (pipes)
			 (multiple-value-bind (heads tails) (heads+tails pipes nil nil)
			   (if (null heads) +empty-pipe+
				   (pipe (apply function heads)
						 (map-n tails))))))
	(if (null more) (map-1 pipe)
		(if (null (cdr more)) (map-2 pipe (car more))
			(map-n (cons pipe more))))))


(defun filter-pipe (predicate pipe)
  "filter-pipe PREDICATE PIPE => NEW-PIPE

This function returns a pipe, whose head values are those of PIPE, for
which PREDICATE returns true."
  (loop
	 :for p := pipe :then (pipe-tail p)
	 :until (empty-pipe-p p)
	 :do (let ((head (pipe-head p)))
		   (when (funcall predicate head)
			 (return (pipe head (filter-pipe predicate (pipe-tail p))))))
	 :finally (return +empty-pipe+)))


(defmacro do-pipe ((var pipe &optional result) &body body)
  "do-pipe (VAR PIPE &optional RESULT) &body BODY => VALUE

This macro provides a way to iterate over the head values of a pipe.
It evaluates the form supplied as PIPE. It then binds VAR to each head
value in turn, and evaluates the forms in BODY like progn. After all 
values have been processed, the do-pipe form evaluates the RESULT form, 
whose value(s) will be returned as the value(s) of do-pipe.

Note, that an implicit anonymous block surrounds the entire do-pipe
form, which can be used by BODY in order to stop iteration early."
  (let ((temp (gensym)))
	`(loop
		:for ,temp := ,pipe :then (pipe-tail ,temp)
		:until (empty-pipe-p ,temp)
		:do (let ((,var (pipe-head ,temp))) ,@body)
		:finally (return ,result))))


(defun list->pipe (list)
  "list->pipe LIST => PIPE

Returns a pipe, which yields all the elements in LIST."
  (if (null list)
	  +empty-pipe+
	  (pipe (car list) (list->pipe (cdr list)))))


(defun vector->pipe (vector &optional (start 0) (end nil))
  "vector->pipe VECTOR &optional START END => PIPE

Returns a pipe, which yields all the elements in VECTOR between indices
START (inclusive, defaults to 0) and END (exclusive, defaults to the
vector's length)."
  (let ((end (or end (length vector))))
	(if (>= start end)
		+empty-pipe+
		(pipe (aref vector start)
			  (vector->pipe vector (1+ start) end)))))


(defun zip-2-pipes (pipe1 pipe2)
  "zip-2-pipes PIPE1 PIPE2 => NEW-PIPE

Returns a pipe, whose head values are the conses of the head values of
PIPE1 and PIPE2. The new pipe returns will stop as soon as one of its
input pipes is exhausted."
  (cond ((empty-pipe-p pipe1) +empty-pipe+)
		((empty-pipe-p pipe2) +empty-pipe+)
		(t (pipe (cons (pipe-head pipe1)
					   (pipe-head pipe2))
				 (zip-pipes (pipe-tail pipe1)
							(pipe-tail pipe2))))))


(defun zip-pipes (pipe1 &rest more)
  "zip-pipes PIPE1 &rest MORE => NEW-PIPE

Returns a pipe, whose head values are lists of the head values of each
input pipe supplied. The new pipe returns will stop as soon as one of 
its input pipes is exhausted."
  (apply #'map-pipe #'list pipe1 more))


(defun pipe->list (pipe)
  "pipe->list PIPE => LIST

Collects all head values of PIPE into a fresh list. If PIPE is infinite,
this function will never return, and is likely to require huge amounts
of precious heap space..."
  (if (empty-pipe-p pipe)
	  '()
	  (let* ((head (cons (pipe-head pipe) nil))
			 (tail head))
		(do-pipe (elt (pipe-tail pipe) head)
		  (setf tail (setf (cdr tail) (cons elt nil)))))))


(defun concatenate-pipes (pipe &rest more-pipes)
  "concatenate-pipes PIPE &rest MORE-PIPES => NEW-PIPE

This function returns a new pipe, whose head values are the head
values of PIPE, followed by the head values of the pipes provided as
MORE-PIPES. After each input pipe is exhausted, the concatenated
pipe will use then next provided input pipe, and so on, until all
pipes have been consumed.

The pipe returned by this function is be infinite, if at least one 
of its input pipes is."
  (labels ((conc (pipe more)
			 (if (empty-pipe-p pipe) 
				 (if (null more) 
					 +empty-pipe+
					 (conc (car more) (cdr more)))
				 (pipe (pipe-head pipe)
					   (conc (pipe-tail pipe) more)))))
	(conc pipe more-pipes)))



(defun drop-from-pipe-while (predicate pipe)
  "drop-from-pipe-while PREDICTE PIPE => ANSWER

Returns a pipe, whose elements are the elements of PIPE, from which
all initial elements have been removed, which match PREDICATE. The
resulting pipe may be empty, if all elements in PIPE match the
predicate."
  (if (empty-pipe-p pipe)
	  +empty-pipe+
	  (let ((head (pipe-head pipe)))
		(if (funcall predicate head)
			(drop-from-pipe-while predicate (pipe-tail pipe))
			pipe))))


(defun take-from-pipe (n pipe)
  (cond
	((<= n 0) +empty-pipe+)
	((empty-pipe-p pipe) +empty-pipe+)
	(t (pipe (pipe-head pipe) (take-from-pipe (- n 1) (pipe-tail pipe))))))


(defun drop-from-pipe (n pipe)
  (loop
	 :for k :upfrom 0 :below n
	 :for p := pipe :then (pipe-tail p)
	 :until (empty-pipe-p p)
	 :finally (return p)))


(defun take-from-pipe-while (predicate pipe)
  "take-from-pipe-while PREDICATE PIPE => ANSWER

Returns a pipe, whose elements are the initial elements of PIPE,
which match PREDICATE. The pipe may be empty, if not even the
first element of PIPE matches PREDICATE (or if PIPE is empty 
itself)."
  (if (empty-pipe-p pipe)
	  +empty-pipe+
	  (let ((head (pipe-head pipe)))
		(if (funcall predicate head)
			(pipe head (take-from-pipe-while predicate (pipe-tail pipe)))
			+empty-pipe+))))


(defun constant-pipe (value &rest more)
  "constant-pipe VALUE &rest MORE => PIPE

returns an infinite pipe, whose head values are the values provided;
after the last value is generated, the pipe starts again at the first
value VALUE."
  (labels ((build (rest first)
			 (if (null rest)
				 first
				 (%make-pair-pipe (car rest) (build (cdr rest) first) t))))
	(let ((first (%make-pair-pipe value nil t)))
	  (setf (pair-pipe-%tail first) (build more first))
	  first)))


(defun fold-pipe-left (function seed pipe)
  (labels ((fold (pipe value)
			 (if (empty-pipe-p pipe) value
				 (fold (pipe-tail pipe)
					   (funcall function seed (pipe-head pipe))))))
	(fold pipe seed)))


(defun reduce-pipe (function pipe &optional (default nil got-default))
  (if (empty-pipe-p pipe)
	  (if got-default default (error "empty pipe"))
	  (let ((head (pipe-head pipe))
			(tail (pipe-tail pipe)))
		(fold-pipe-left function head tail))))


#|(defun constant-pipe* (value &rest more)
  (labels ((continuation (head list)
			 #'(lambda ()
				 (if (null list) head
					 (%make-pair-pipe (car list) (continuation head (cdr list)))))))
	(let ((link (%make-pair-pipe value +empty-pipe+ nil)))
	  (if (null more)
		  (setf (pair-pipe-%tail link) link
				(pair-pipe-forced link) t)
		  (setf (pair-pipe-%tail link) (continuation link more)))
	  link)))
|#

(defun stepper (start end test increment)
  (if (funcall test start end)
	  (pipe start (stepper (+ start increment) end test increment))
	  +empty-pipe+))

(defun true (&rest unused)
  (declare (ignore unused))
  t)


(defun make-counting-pipe (&key (from 0) (up-by 1 have-up) (down-by 1 have-down) 
					       above below to)
  (check-type above (or null real))
  (check-type below (or null real))
  (check-type to (or null real))
  (check-type from real)
  (when (and have-up have-down) (error "cannot use both, ~S and ~S" :up-by :down-by))
  (if (or have-up (not have-down))
	  (progn 
		(check-type up-by (and real (satisfies plusp)))
		(when above (error "~S cannot be used with ~S" :above :up-by))
		(when (and below to) (error "cannot use both, ~S and ~S" :below :to))
		(cond 
		  (below
		   (when (< below from) (error "stop value ~S is below start value ~S" below from))
		   (stepper from below #'< up-by))
		  (to 
		   (when (< to from) (error "stop value ~S is below start value ~S" to from))
		   (stepper from to #'<= up-by))
		  (t (stepper from 0 #'true up-by))))
	  (progn 
		(check-type down-by (and real (satisfies plusp)))
		(when below (error "~S cannot be used with ~S" :below :down-by))
		(when (and above to) (error "cannot use both, ~S and ~S" :above :to))
		(cond 
		  (above 
		   (when (> above from) (error "stop value ~S is above start value ~S" above from))
		   (stepper from above #'> (- down-by)))
		  (to
		   (when (> to from) (error "stop value ~S is above start value ~S" to from)) 
		   (stepper from to #'>= (- down-by)))
		  (t (stepper from 0 #'true (- down-by)))))))


(defun character-stream->pipe (stream)
  "character-stream->pipe STREAM => PIPE

Generates a pipe, which yields the characters from STREAM sequentially. It
yields an empty pipe, when the end of the stream has been reached. Note, that
this function will have side-effects on the underlying stream, in particular,
it will advance the stream's read pointer."
  (let ((head (read-char stream nil)))
	(if (null head) +empty-pipe+
		(pipe head (character-stream->pipe stream)))))


(defun byte-stream->pipe (stream)
  "byte-stream->pipe STREAM => PIPE

Generates a pipe, which yields the bytes from STREAM sequentially. It
yields an empty pipe, when the end of the stream has been reached. Note, that
this function will have side-effects on the underlying stream, in particular,
it will advance the stream's read pointer."
  (let ((head (read-byte stream nil)))
	(if (null head) +empty-pipe+
		(pipe head (byte-stream->pipe stream)))))


(defun object-stream->pipe (stream)
  "object-stream->pipe STREAM => PIPE

Generates a pipe, which yields the Lisp objects from STREAM sequentially. It
yields an empty pipe, when the end of the stream has been reached. Note, that
this function will have side-effects on the underlying stream, in particular,
it will advance the stream's read pointer.

This function will capture the values of the special variables 

  *readtable*
  *read-default-float-format*
  *read-base*
  *read-suppress*
  *package*
  *read-eval*

at the time of the call to object-stream->pipe, and use this value for all
subsequent read operations on the given STREAM."
  (let ((readtable *readtable*)
		(default-float-format *read-default-float-format*)
		(read-base *read-base*)
		(read-suppress *read-suppress*)
		(package *package*)
		(read-eval *read-eval*)
		(marker (cons nil nil)))
	(labels ((read-next ()
			   (let ((*readtable* readtable)
					 (*read-base* read-base)
					 (*read-suppress* read-suppress)
					 (*package* package)
					 (*read-default-float-format* default-float-format)
					 (*read-eval* read-eval))
				 (let ((head (read stream nil marker)))
				   (if (eq head marker) +empty-pipe+
					   (%make-pair-pipe head #'read-next nil))))))
	  (read-next))))


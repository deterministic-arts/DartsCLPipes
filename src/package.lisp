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

(in-package "COMMON-LISP-USER")

(defpackage "DARTS.LIB.PIPES"
  (:use "COMMON-LISP")
  (:export "PIPE" "EMPTY-PIPE-P" "PIPE-HEAD" "PIPE-TAIL"
		   "MAP-PIPE" "FILTER-PIPE" "DO-PIPE" "ZIP-PIPES"
		   "LIST->PIPE" "VECTOR->PIPE" "PIPE->LIST" "ZIP-2-PIPES"
		   "EMPTY-PIPE" "CONSTANT-PIPE" "MAKE-PIPE" "MAKE-COUNTING-PIPE"
		   "CONCATENATE-PIPES" "TAKE-FROM-PIPE-WHILE" "TAKE-FROM-PIPE"
		   "DROP-FROM-PIPE" "DROP-FROM-PIPE-WHILE" "CHARACTER-STREAM->PIPE"
		   "BYTE-STREAM->PIPE" "OBJECT-STREAM->PIPE" "FOLD-PIPE-LEFT"
		   "REDUCE-PIPE"
		   "HEAD" "TAIL" "EMPTYP" "FOLD-LEFT" "FOLD-RIGHT"))

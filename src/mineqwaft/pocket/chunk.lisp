;;;; Mineqwaft - a Minecraft server in lisp
;;;;
;;;; Copyright (c) 2014, Daniel Parnell <me@danielparnell.com>
;;;;
;;;; Permission is hereby granted, free of charge, to any person
;;;; obtaining a copy of this software and associated documentation
;;;; files (the "Software"), to deal in the Software without
;;;; restriction, including without limitation the rights to use, copy,
;;;; modify, merge, publish, distribute, sublicense, and/or sell copies
;;;; of the Software, and to permit persons to whom the Software is
;;;; furnished to do so, subject to the following conditions:
;;;;
;;;; The above copyright notice and this permission notice shall be
;;;; included in all copies or substantial portions of the Software.
;;;;
;;;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;;;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;;;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;;;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;;;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;;;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;;;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;;;; DEALINGS IN THE SOFTWARE.
;;;;

(in-package :mineqwaft-pocket)

(defclass chunk ()
  ((x :accessor chunk-x)
   (z :accessor chunk-z)
   (blocks :accessor chunk-blocks)))

(defmethod chunk-set-block ((chunk chunk) x y z b)
  (declare (type (unsigned-byte 8) x y z b))
  (setf (aref (chunk-blocks chunk) (+ (logand y 127) (* 128 (+ (logand x 15) (* (logand z 15) 16))))) b))

(defmethod initialize-instance :after ((chunk chunk) &key x z)
  (setf (chunk-x chunk) x)
  (setf (chunk-z chunk) z)
  (setf (chunk-blocks chunk) (make-array (* 16 16 128) :element-type '(unsigned-byte 8) :initial-element 0))

  ;; set up some default blocks
  (loop
     for x from 0 to 15
     for y from 0 to 99
     for z from 0 to 15
     do (chunk-set-block chunk x y z 1)))

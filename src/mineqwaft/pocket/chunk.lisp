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
  ((x :accessor chunk-x :documentation "The x position of the chunk")
   (z :accessor chunk-z :documentation "The z position of the chunk")
   (blocks :accessor chunk-blocks :documentation "A 16 x 16 array of unsigned-byte representing the blocks.")
   (data :accessor chunk-data :documentation "An 16 x 16 x 128 array of unsigned-byte representing the data")
   (sky-light :accessor chunk-sky-light :documentation "An 16 x 16 x 128 array of unsigned-byte representing the light from the sky")
   (light :accessor chunk-light :documentation "An 16 x 16 x 128 array of unsigned-byte representing the light from a block")))

(defgeneric chunk-set-block (chunk x y z b))
(defgeneric chunk-set-data (chunk x y z d))
(defgeneric chunk-set-sky-light (chunk x y z light))
(defgeneric chunk-set-block-light (chunk x y z light))

(defmethod chunk-set-block ((chunk chunk) x y z b)
  (declare (type (unsigned-byte 8) x y z b))
  (setf (aref (chunk-blocks chunk) (+ y (* 128 (+ (logand x 15) (* (logand z 15) 16))))) b))

(defmethod chunk-set-data((chunk chunk) x y z d)
  (declare (type (unsigned-byte 8) x y z d))
  (if (= (logand y) 0)
      (setf (ldb (byte 4 0) (aref (chunk-data chunk) (+ (ash y -1) (* 64 (+ (logand x 15) (* (logand z 15) 16)))))) d)
      (setf (ldb (byte 4 4) (aref (chunk-data chunk) (+ (ash y -1) (* 64 (+ (logand x 15) (* (logand z 15) 16)))))) d)))

(defmethod chunk-set-sky-light((chunk chunk) x y z light)
  (declare (type (unsigned-byte 8) x y z light))
  (if (= (logand y) 0)
      (setf (ldb (byte 4 0) (aref (chunk-sky-light chunk) (+ (ash y -1) (* 64 (+ (logand x 15) (* (logand z 15) 16)))))) light)
      (setf (ldb (byte 4 4) (aref (chunk-sky-light chunk) (+ (ash y -1) (* 64 (+ (logand x 15) (* (logand z 15) 16)))))) light)))

(defmethod chunk-set-light((chunk chunk) x y z light)
  (declare (type (unsigned-byte 8) x y z light))
  (if (= (logand y) 0)
      (setf (ldb (byte 4 0) (aref (chunk-light chunk) (+ (ash y -1) (* 64 (+ (logand x 15) (* (logand z 15) 16)))))) light)
      (setf (ldb (byte 4 4) (aref (chunk-light chunk) (+ (ash y -1) (* 64 (+ (logand x 15) (* (logand z 15) 16)))))) light)))

(defmethod initialize-instance :after ((chunk chunk) &key x z)
  (setf (chunk-x chunk) (or x 0))
  (setf (chunk-z chunk) (or z 0))
  (setf (chunk-blocks chunk) (make-array (* 16 16 128) :element-type '(unsigned-byte 8) :initial-element 0))
  (setf (chunk-data chunk) (make-array (* 16 16 128) :element-type '(unsigned-byte 8) :initial-element 0))
  (setf (chunk-sky-light chunk) (make-array (* 16 16 128) :element-type '(unsigned-byte 8) :initial-element 0))
  (setf (chunk-light chunk) (make-array (* 16 16 128) :element-type '(unsigned-byte 8) :initial-element 0))

  ;; set up some default blocks
  (loop
     for x from 0 to 15
     for y from 0 to 99
     for z from 0 to 15
     do
       (chunk-set-block chunk x y z 1)
       (if (= y 99) (chunk-set-sky-light x y z 15))))

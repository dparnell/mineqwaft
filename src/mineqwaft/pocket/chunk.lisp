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
   (data :accessor chunk-data :documentation "A 16 x 16 x 128 array of unsigned-byte representing the data")
   (sky-light :accessor chunk-sky-light :documentation "A 16 x 16 x 128 array of unsigned-byte representing the light from the sky")
   (light :accessor chunk-light :documentation "A 16 x 16 x 128 array of unsigned-byte representing the light from a block")
   (biomes :accessor chunk-biomes)
   (biome-colours :accessor chunk-biome-colours)))

(defun encode-chunk (chunk)
  (let ((data (concatenate 'vector
                                     (raknet-data:int-lvalue (chunk-x chunk))
                                     (raknet-data:int-lvalue (chunk-z chunk))
                                     (chunk-blocks chunk)
                                     (chunk-data chunk)
                                     (chunk-sky-light chunk)
                                     (chunk-light chunk)
                                     (chunk-biomes chunk)
                                     (chunk-biome-colours chunk))))

    (salza2:compress-data (make-array (length data) :element-type '(unsigned-byte 8) :initial-contents data) 'salza2:deflate-compressor)))

(defgeneric chunk-set-block (chunk x y z b))
(defgeneric chunk-set-data (chunk x y z d))
(defgeneric chunk-set-sky-light (chunk x y z light))
(defgeneric chunk-set-block-light (chunk x y z light))
(defgeneric chunk-set-biome-colour (chunk x z r g b))

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

(defmethod chunk-set-biome-colour ((chunk chunk) x z r g b)
    (let ((colours (chunk-biome-colours chunk))
          (i (* (+ x (* z 16)) 4)))
      (setf (aref colours (+ 0 i)) b)
      (setf (aref colours (+ 1 i)) g)
      (setf (aref colours (+ 2 i)) r)))

(defmethod initialize-instance :after ((chunk chunk) &key x z)
  (setf (chunk-x chunk) (or x 0))
  (setf (chunk-z chunk) (or z 0))
  (setf (chunk-blocks chunk) (make-array (* 16 16 128) :element-type '(unsigned-byte 8) :initial-element 0))
  (setf (chunk-data chunk) (make-array (* 16 16 128) :element-type '(unsigned-byte 8) :initial-element 0))
  (setf (chunk-sky-light chunk) (make-array (* 16 16 128) :element-type '(unsigned-byte 8) :initial-element 0))
  (setf (chunk-light chunk) (make-array (* 16 16 128) :element-type '(unsigned-byte 8) :initial-element 0))
  (setf (chunk-biomes chunk) (make-array 256 :element-type '(unsigned-byte 8) :initial-element 1))
  (setf (chunk-biome-colours chunk) (make-array 1024 :element-type '(unsigned-byte 8) :initial-element 0))

  ;; the default colours
  (loop for x from 0 to 15 for z from 0 to 15 do (chunk-set-biome-colour chunk x z #x85 #xb2 #x4a))

  ;; set up some default blocks
  (loop
     for x from 0 to 15
     for y from 0 to 99
     for z from 0 to 15
     do
       (chunk-set-block chunk x y z 1)
       (if (= y 99) (chunk-set-sky-light x y z 15))))

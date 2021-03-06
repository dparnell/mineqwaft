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

(in-package :raknet-data)

(defun hex-dump (packet)
  (loop for v across packet
     collect (write-to-string v :base 16)))

(defun short-value (short)
  (let ((high (ash short -8)) (low (logand short 255)))
    (list high low)))

(defun int-value (int32)
  (list (ash int32 -24)
        (logand (ash int32 -16) #xff)
        (logand (ash int32 -8) #xff)
        (logand int32 #xff)))

(defun int-lvalue (int32)
  (list (logand int32 #xff)
        (logand (ash int32 -8) #xff)
        (logand (ash int32 -16) #xff)
        (ash int32 -24)))

(defun float-value (f)
  (int-value (ieee-floats:encode-float32 (float f))))

(defun triad-value (int32)
  (list (logand int32 #xff)
        (logand (ash int32 -8) #xff)
        (logand (ash int32 -16) #xff)))

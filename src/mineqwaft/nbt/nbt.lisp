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

(in-package :nbt)

(defparameter +little-endian+ 0)
(defparameter +big-endian+ 1)
(defvar *file-endianess* +big-endian+)

(defparameter +tag-end+ 0)
(defparameter +tag-byte+ 1)
(defparameter +tag-short+ 2)
(defparameter +tag-int+ 3)
(defparameter +tag-long+ 4)
(defparameter +tag-float+ 5)
(defparameter +tag-double+ 6)
(defparameter +tag-byte-array+ 7)
(defparameter +tag-string+ 8)
(defparameter +tag-enum+ 9)
(defparameter +tag-compound+ 10)
(defparameter +tag-int-array+ 11)

(defun put-short (value stream)
  (let ((bytes (list (logand #xff value) (logand #xff (ash value -8)))))
    (write-sequence (if (= *file-endianess* +big-endian+)
        (nreverse bytes)
        bytes) stream)))

(defun put-int (value stream)
  (let ((bytes (list (logand #xff value)
                     (logand #xff (ash value -8))
                     (logand #xff (ash value -16))
                     (logand #xff (ash value -24)))))
    (write-sequence (if (= *file-endianess* +big-endian+)
                        (nreverse bytes)
                        bytes) stream)))

(defun put-long (value stream)
  (let ((bytes (list (logand #xff value)
                     (logand #xff (ash value -8))
                     (logand #xff (ash value -16))
                     (logand #xff (ash value -24))
                     (logand #xff (ash value -32))
                     (logand #xff (ash value -40))
                     (logand #xff (ash value -48))
                     (logand #xff (ash value -56)))))
    (write-sequence (if (= *file-endianess* +big-endian+)
                        (nreverse bytes)
                        bytes) stream)))

(defun put-float (value stream)
  (put-int (ieee-floats:encode-float32 (float value)) stream))

(defun put-double (value stream)
  (put-long (ieee-floats:encode-float64 (float value)) stream))

(defclass tag () ())

(defmethod write-tag ((tag tag) stream)
  (declare (ignore stream))
  (error "write-tag not implemented for: ~A" tag))

(defclass end-tag (tag) ())

(defmethod write-tag ((tag end-tag) stream)
  (write-byte +tag-end+ stream))

(defclass named-tag (tag)
  ((name :accessor tag-name :initarg :name)
  (value :accessor tag-value :initarg :value)))

(defmethod write-tag ((tag named-tag) stream)
  (let* ((name (tag-name tag))
         (name-length (length name)))

    (put-short name-length stream)
    (write-sequence name stream)))


(defclass byte-tag (named-tag) ())

(defmethod write-tag ((tag byte-tag) stream)
  (write-byte +tag-byte+ stream)
  (call-next-method)
  (write-byte (tag-value tag) stream))


(defclass short-tag (named-tag) ())

(defmethod write-tag ((tag short-tag) stream)
  (write-byte +tag-short+ stream)
  (call-next-method)
  (put-short (tag-value tag) stream))

(defclass int-tag (named-tag) ())

(defmethod write-tag ((tag int-tag) stream)
  (write-byte +tag-int+ stream)
  (call-next-method)
  (put-int (tag-value tag) stream))

(defclass long-tag (named-tag) ())

(defmethod write-tag ((tag long-tag) stream)
  (write-byte +tag-long+ stream)
  (call-next-method)
  (put-long (tag-value tag) stream))

(defclass float-tag (named-tag) ())

(defmethod write-tag ((tag float-tag) stream)
  (write-byte +tag-float+ stream)
  (call-next-method)
  (put-float (tag-value tag) stream))

(defclass double-tag (named-tag) ())

(defmethod write-tag ((tag double-tag) stream)
  (write-byte +tag-double+ stream)
  (call-next-method)
  (put-double (tag-value tag) stream))

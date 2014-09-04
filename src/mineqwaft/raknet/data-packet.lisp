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

(defclass data-packet ()
  ((packet-type :accessor packet-type :initarg :packet-type)
   (body :accessor packet-body :initarg :body)
   (offset :accessor packet-offset :initform 0)))

;; an array of classes representing the different types of packets
(defvar *packet-types*
  (make-array 256 :initial-element 'data-packet))

(defun register-packet-type (id packet-class)
  (setf (aref *packet-types* id) packet-class))

(defmethod get-bytes ((packet data-packet) count)
  (let ((result (subseq (packet-body packet) (packet-offset packet) (+ (packet-offset packet) count))))
    (setf (packet-offset packet) (+ (packet-offset packet) count))

    result))

(defmethod get-byte ((packet data-packet))
  (aref (get-bytes packet 1) 0))

(defmethod get-short ((packet data-packet))
  (let ((bytes (get-bytes packet 2)))
    (logior (ash (aref bytes 0) 8) (aref bytes 1))))

(defmethod get-int ((packet data-packet))
  (let ((bytes (get-bytes packet 4)))
    (logior (ash (aref bytes 0) 24)
            (ash (aref bytes 1) 16)
            (ash (aref bytes 2) 8)
            (aref bytes 3))))

(defmethod get-long ((packet data-packet))
  (logior (ash (get-int packet) 32) (get-int packet)))

(defmethod get-float ((packet data-packet))
  (ieee-floats:decode-float32 (get-int packet)))

(defmethod get-string ((packet data-packet))
  (let ((length (get-short packet)))
    (flexi-streams:octets-to-string (get-bytes packet length))))

(defmethod decode-data-packet ((packet data-packet))
  (format t "Unhandled encapsulated packet: ~A~%" (hex-dump (packet-body packet)))
  packet)

(defmethod handle-data-packet ((packet data-packet) src-host src-port)
  (declare (ignore src-host src-port))
  nil)

(defclass ignored-data-packet (data-packet) ())

(defmethod decode-data-packet ((packet ignored-data-packet))
  packet)

;; PING packet
(register-packet-type #x00 'ignored-data-packet)

;; PONG packet
(register-packet-type #x03 'ignored-data-packet)

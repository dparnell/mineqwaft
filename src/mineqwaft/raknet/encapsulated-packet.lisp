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

(in-package :raknet)

(defclass encapsulated-packet ()
  ((reliability :accessor packet-reliability
                :initarg :reliability
                :initform 0)
   (has-split :accessor packet-has-split
              :initarg :has-split
              :initform nil)
   (length :accessor packet-length
           :initarg :length
           :initform 0)
   (message-index :accessor packet-message-index
                  :initarg :message-index
                  :initform nil)
   (order-index :accessor packet-order-index
                :initarg :order-index
                :initform 0)
   (order-channel :accessor packet-order-channel
                  :initarg :order-channel
                  :initform 0)
   (split-count :accessor packet-split-count
                :initarg :split-count
                :initform 0)
   (split-id :accessor packet-split-id
             :initarg :split-id
             :initform 0)
   (split-index :accessor packet-split-index
             :initarg :split-index
             :initform 0)
   (body :accessor packet-body
           :initarg :body
           :initform nil)
   (need-ack :accessor packet-need-ack
             :initarg :need-ack
             :initform nil)
   (identifier-ack :accessor packet-identifier-ack
                   :initarg :identifier-ack
                   :initform nil)))

(defgeneric total-packet-length (packet))

(defmethod total-packet-length ((packet encapsulated-packet))
  (+ 3 (length (packet-body body)) (if (packet-message-index packet) 3 0) (if (packet-order-index packet) 4 0) (if (packet-has-split packet) 9 0)))

(defmethod total-packet-length (packet)
  (length packet))

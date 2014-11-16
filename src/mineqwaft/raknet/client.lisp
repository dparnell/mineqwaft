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

(defvar *clients* (make-hash-table))

(defclass client ()
  ((host :accessor client-host :initarg :host)
   (port :accessor client-port :initarg :post)
   (last-seen :accessor client-last-seen)
   (buffer :accessor client-buffer)
   (seq-number :accessor client-seq-number :initform 0)))

(defun write-short (short stream)
  (write-byte (ldb (byte 8 0) short) stream)
  (write-byte (ldb (byte 8 8) short) stream))

(defun write-triad (triad stream)
  (write-byte (ldb (byte 8 0) triad) stream)
  (write-byte (ldb (byte 8 8) triad) stream)
  (write-byte (ldb (byte 8 16) triad) stream))

(defun write-int (int stream)
  (write-byte (ldb (byte 8 0) int) stream)
  (write-byte (ldb (byte 8 8) int) stream)
  (write-byte (ldb (byte 8 16) int) stream)
  (write-byte (ldb (byte 8 24) int) stream))

(defgeneric encode-packet (packet stream))

(defmethod encode-packet ((packet encapsulated-packet) stream)
  (let ((body (packet-body packet))
        (reliability (packet-reliability packet)))
    (write-byte (logior (ash (packet-reliability packet) 5) (if (packet-has-split packet) #x10 0)) stream)
    (write-short (* (length body) 8) stream)

    (if (or (= reliability 2) (= reliability 3) (= reliability 4) (= reliability 6) (= reliability 7))
        (write-triad (packet-message-index packet) stream))

    (if (or (= reliability 1) (= reliability 3) (= reliability 4) (= reliability 7))
        (progn
          (write-triad (packet-order-index packet) stream)
          (write-byte (packet-order-channel packet) stream)))

    (if (packet-has-split packet)
        (progn
          (write-int (packet-split-count packet) stream)
          (write-short (packet-split-id packet) stream)
          (write-int (packet-split-index packet) stream)))

    (write-sequence body stream)))

(defmethod encode-packet (packet stream)
  (write-sequence packet stream))

(defun send-encapsulated-packet (client packet-type packet)
  (let ((buffer (flexi-streams:make-in-memory-output-stream :element-type 'octet))
        (seq (incf (client-seq-number client))))

    (write-byte packet-type buffer)
    (write-triad seq buffer)
    (encode-packet packet buffer)

    (let ((data (flexi-streams:get-output-stream-sequence buffer)))
      (usocket:socket-send *socket* data (length data) :host (client-host client) :port (client-port client)))))

(defun client-seen (client)
  (setf (client-last-seen client) (get-universal-time)))

(defmethod initialize-instance :after ((client client) &key)
  (client-seen client)
  (setf (client-buffer client) (flexi-streams:make-in-memory-output-stream :element-type 'octet))

  ;; save this client off into the *clients* hash so we can find it again later
  (setf (gethash (list (client-host client) (client-port client)) *clients*) client))



(defmethod queue-packet (client packet &key send-now)
  (if send-now
      (send-encapsulated-packet client #x80 packet)

      (let ((length (total-packet-length packet))
            (buffer-length (flexi-streams:output-stream-sequence-length (client-buffer client))))
        ;; will adding this packet make the currently queued data no longer fit in a single UDP packet?
        (if (and (> buffer-length 0) (> (+ length buffer-length) +mtu-size+))
            (send-encapsulated-packet client #x84 (flexi-streams:get-output-stream-sequence (client-buffer client))))
        (encode-packet packet (client-buffer client)))))

(defun find-client (host port)
  (gethash (list host port) *clients*))

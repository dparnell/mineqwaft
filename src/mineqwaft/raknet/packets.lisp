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

(defparameter *client-added-callback* nil)
(defparameter *client-connected-callback* nil)

(defun ignore-packet (src-host src-port packet)
  (declare (ignore src-host src-port packet))
  nil)

(defun unknown-packet (src-host src-port packet)
  (print (format t "Got unknown packet from ~A on port ~A of type ~A bytes: ~A" src-host src-port (aref packet 0) packet))
  nil)

(defun unknown-encapsulated-packet (src-host src-port packet)
  (print (format t "Got unknown encapsulated packet from ~A on port ~A of type ~A bytes: ~A" src-host src-port (aref packet 0) packet))
  nil)

;; an array of functions for handling packets
(defvar *packet-handlers*
  (make-array 256 :initial-element 'unknown-packet))

;; an array of functions for handling packets
(defvar *encapsulated-packet-handlers*
  (make-array 256 :initial-element 'unknown-encapsulated-packet))

;; add a packet handler for a given packet type
(defun add-packet-handler (id fn)
  (setf (aref *packet-handlers* id) fn))

(defun add-encapsulated-packet-handler (id fn)
  (setf (aref *encapsulated-packet-handlers* id) fn))

(defun handle-packet (src-host src-port packet)
  (print (format nil "Got packet ~A" packet))
  (print (format nil " from ~A on port ~A" src-host src-port))

  (funcall (aref *packet-handlers* (aref packet 0)) src-host src-port packet))

(defparameter +magic+
  #(#x00 #xff #xff #x00 #xfe #xfe #xfe #xfe #xfd #xfd #xfd #xfd #x12 #x34 #x56 #x78))

(defparameter +server-id+
  #(#x00 #x00 #x00 #x00 #x37 #x2c #xdc #x9e))

(defun short-value (short)
  (let ((high (ash short -8)) (low (logand short 255)))
        (list high low)))

(defun get-short (data pos)
  (+ (aref data (+ pos 1)) (ash (aref data pos) 8)))

;; ID_CONNECTED_PING_OPEN_CONNECTIONS
(add-packet-handler #x01 (lambda (src-host src-port packet)
                           (concatenate 'vector
                                        #( #x1c )
                                        (subseq packet 1 9)
                                        +server-id+
                                        +magic+
                                        (short-value (length *server-name*))
                                        (arnesi:string-to-octets *server-name* :utf8))))

;; ID_OPEN_CONNECTION_REQUEST_1
(add-packet-handler #x05 (lambda (src-host src-port packet)
                           (concatenate 'vector
                                        #( #x06 )
                                        +magic+
                                        +server-id+
                                        #( #x00 )
                                        (short-value 1447))))

;; ID_OPEN_CONNECTION_REQUEST_2
(add-packet-handler #x07 (lambda (src-host src-port packet)
                           (if *client-added-callback* (funcall *client-added-callback* src-host src-port (subseq packet 25 33)))

                           (concatenate 'vector
                                        #( #x08 )
                                        +magic+
                                        +server-id+
                                        (short-value src-port)
                                        (short-value 1464)
                                        #( #x00 ))))

(defun decode-encapsulated-body (data acc)
  (if (= (length data) 0)
    acc
    (let* ((encapsulation-id (aref data 0))
           (packet-length (/ (get-short data 1) 8))
           (offset (cond
                    ((= encapsulation-id #x00) 3)
                    ((= encapsulation-id #x40) 6)
                    ((= encapsulation-id #x60) 10))))

      (decode-encapsulated-body (subseq data (+ packet-length offset)) (cons (subseq data offset (+ packet-length offset)) acc)))))

(defun split-encapsulated-packet (data)
  (nreverse (decode-encapsulated-body (subseq data 4) nil)))


(defun handle-encapsulated-packet (src-host src-port packet)
  (let ((replies (apply #'concatenate (cons 'vector (remove nil (mapcar (lambda (part)
                                                                          (funcall (aref *encapsulated-packet-handlers* (aref part 0)) src-host src-port part))
                                                                        (split-encapsulated-packet packet)))))))

    (if (= 0 (length replies))
        nil
        (concatenate 'vector
                     #( #x80 )
                     #( #x00 #x00 #x00 )
                     #( #x00 )
                     (short-value (* (length replies) 8))
                     replies))))


(add-packet-handler #x80 'handle-encapsulated-packet)
(add-packet-handler #x81 'handle-encapsulated-packet)
(add-packet-handler #x82 'handle-encapsulated-packet)
(add-packet-handler #x83 'handle-encapsulated-packet)
(add-packet-handler #x84 'handle-encapsulated-packet)
(add-packet-handler #x85 'handle-encapsulated-packet)
(add-packet-handler #x86 'handle-encapsulated-packet)
(add-packet-handler #x87 'handle-encapsulated-packet)
(add-packet-handler #x88 'handle-encapsulated-packet)
(add-packet-handler #x89 'handle-encapsulated-packet)
(add-packet-handler #x8A 'handle-encapsulated-packet)
(add-packet-handler #x8B 'handle-encapsulated-packet)
(add-packet-handler #x8C 'handle-encapsulated-packet)
(add-packet-handler #x8D 'handle-encapsulated-packet)
(add-packet-handler #x8E 'handle-encapsulated-packet)
(add-packet-handler #x8F 'handle-encapsulated-packet)

;; ACK
(add-packet-handler #xC0 (lambda (src-host src-port packet)
                           (declare (ignore src-host src-port packet))
                           (print "GOT ACK!")
                           nil))

;; PING
(add-encapsulated-packet-handler #x00 'ignore-packet)

;; PONG
(add-encapsulated-packet-handler #x03 'ignore-packet)

;; CLIENT_CONNECT
(add-encapsulated-packet-handler #x09 (lambda (src-host src-port packet)
                                        (declare (ignore src-host))
                                        (concatenate 'vector
                                                     #( #x10 )
                                                     #( #x04 #x3f #x57 #xfe )
                                                     #( #xcd )
                                                     (short-value src-port)
                                                     #(
                                                       #x00 #x00 #x04 #xf5 #xff #xff #xf5
                                                       #x00 #x00 #x04 #xff #xff #xff #xff
                                                       #x00 #x00 #x04 #xff #xff #xff #xff
                                                       #x00 #x00 #x04 #xff #xff #xff #xff
                                                       #x00 #x00 #x04 #xff #xff #xff #xff
                                                       #x00 #x00 #x04 #xff #xff #xff #xff
                                                       #x00 #x00 #x04 #xff #xff #xff #xff
                                                       #x00 #x00 #x04 #xff #xff #xff #xff
                                                       #x00 #x00 #x04 #xff #xff #xff #xff
                                                       #x00 #x00 #x04 #xff #xff #xff #xff )
                                                     #( #x00 #x00 )
                                                     (subseq packet 9 17)
                                                     #( #x00 #x00 #x00 #x00 #x04 #x44 #x0b #xa9 ))))

;; CLIENT_HANDSHAKE
(add-encapsulated-packet-handler #x13 (lambda (src-host src-port packet)
                                        (declare (ignore packet))
                                        (if *client-connected-callback* (funcall *client-connected-callback* src-host src-port))
                                        nil))

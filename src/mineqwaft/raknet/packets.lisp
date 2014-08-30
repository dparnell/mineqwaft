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

(defun unknown-packet (src-host src-port packet)
  (print (format t "Got unknown packet from ~A on port ~A of type ~A bytes: ~A" src-host src-port (aref packet 0) packet))
  nil)

;; an array of functions for handling packets
(defvar *packet-handlers*
  (make-array 256 :initial-element 'unknown-packet))

;; add a packet handler for a given packet type
(defun add-packet-handler (id fn)
  (setf (aref *packet-handlers* id) fn))

(defun handle-packet (src-host src-port packet)
;  (print (format t "Got packet ~A" packet))
;  (print (format t " from ~A on port ~A" src-host src-port))

  (funcall (aref *packet-handlers* (aref packet 0)) src-host src-port packet))

(defparameter +magic+
  #(#x00 #xff #xff #x00 #xfe #xfe #xfe #xfe #xfd #xfd #xfd #xfd #x12 #x34 #x56 #x78))

(defparameter +server-id+
  #(#x00 #x00 #x00 #x00 #x37 #x2c #xdc #x9e))

(defun short-value (short)
  (let ((high (ash short -8)) (low (logand short 255)))
        (list high low)))

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

(defun handle-encapsulated-packet (src-host src-port packet)
  nil)

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

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

(defvar *client-added-callback* nil)
(defvar *client-connected-callback* nil)

(defun ignore-packet (socket src-host src-port packet)
  (declare (ignore socket src-host src-port packet))
  nil)

(defun unknown-packet (socket src-host src-port packet)
  (declare (ignore socket))
  (format t "Got unknown packet from ~A on port ~A of type ~A~%bytes: ~A~%" src-host src-port (aref packet 0) (raknet-data:hex-dump packet))
  nil)

;; an array of functions for handling packets
(defvar *packet-handlers*
  (make-array 256 :initial-element 'unknown-packet))

;; add a packet handler for a given packet type
(defun add-packet-handler (id fn)
  (setf (aref *packet-handlers* id) fn))

(defun handle-packet (socket src-host src-port packet)
  (format t "Got packet ~A~%" (raknet-data:hex-dump packet))
  (format t "from ~A on port ~A~%" src-host src-port)

  (funcall (aref *packet-handlers* (aref packet 0)) socket src-host src-port packet))

(defparameter +magic+
  #(#x00 #xff #xff #x00 #xfe #xfe #xfe #xfe #xfd #xfd #xfd #xfd #x12 #x34 #x56 #x78))

(defparameter +server-id+
  #(#x00 #x00 #x00 #x00 #x37 #x2c #xdc #x9e))

(defun get-short (data pos)
  (+ (aref data (+ pos 1)) (ash (aref data pos) 8)))

(defun get-int (data pos)
  (logior (ash (aref data pos) 24)
          (ash (aref data (+ 1 pos)) 16)
          (ash (aref data (+ 2 pos)) 8)
          (aref data (+ 3 pos))))

(defun get-triad (data pos)
  (logior (aref data pos)
          (ash (aref data (+ 1 pos)) 8)
          (ash (aref data (+ 2 pos)) 16)))

;; ID_CONNECTED_PING_OPEN_CONNECTIONS
(add-packet-handler #x01 (lambda (socket src-host src-port packet)
                           (declare (ignore socket src-host src-port))
                           (concatenate 'vector
                                        #( #x1c )
                                        (subseq packet 1 9)
                                        +server-id+
                                        +magic+
                                        (raknet-data:short-value (length *server-name*))
                                        (arnesi:string-to-octets *server-name* :utf8))))

;; ID_OPEN_CONNECTION_REQUEST_1
(add-packet-handler #x05 (lambda (socket src-host src-port packet)
                           (declare (ignore socket src-host src-port packet))
                           (concatenate 'vector
                                        #( #x06 )
                                        +magic+
                                        +server-id+
                                        #( #x00 )
                                        (raknet-data:short-value 1447))))

;; ID_OPEN_CONNECTION_REQUEST_2
(add-packet-handler #x07 (lambda (socket src-host src-port packet)
                           (declare (ignore packet))
                           (if *client-added-callback* (funcall *client-added-callback* socket src-host src-port))

                           (concatenate 'vector
                                        #( #x08 )
                                        +magic+
                                        +server-id+
                                        (raknet-data:short-value src-port)
                                        (raknet-data:short-value 1464)
                                        #( #x00 ))))

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
                  :initform 0)
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

(defun decode-encapsulated-body (data acc &key internal)
  (if (= (length data) 0)
      acc
      (let* ((flags (aref data 0))
             (reliability (ash (logand #xe0 flags) -5))
             (has-split (> (logand #x10 flags) 0))
             (packet-length (if internal
                                (get-int data 1)
                                (/ (get-short data 1) 8)))
             (identifier-ack (if internal
                                 (get-int data 5)
                                 nil))
             (header-length (if internal
                                9
                                3))
             (has-message-index (or (= 2 reliability) (= 3 reliability) (= 4 reliability) (= 6 reliability) (= 7 reliability)))
             (has-order-index (or (= 1 reliability) (= 3 reliability) (= 4 reliability) (= 7 reliability)))
             (message-index (if has-message-index
                                (get-triad data header-length)))
             (message-index-size (if has-message-index 3 0))
             (order-index (if has-order-index
                              (get-triad data (+ header-length message-index-size))
                              nil))
             (order-channel (if has-order-index
                                (aref data (+ header-length message-index-size 3))
                                nil))
             (order-index-size (if has-order-index 4 0))
             (main-header-size (+ header-length message-index-size order-index-size))
             (split-count (if has-split
                              (get-int data main-header-size)
                              0))
             (split-id (if has-split
                           (get-short data (+ main-header-size 4))
                           0))
             (split-index (if has-split
                              (get-int data (+ main-header-size 6))
                              0))
             (split-header-size (if has-split 10 0))
             (offset (+ main-header-size split-header-size))
             (body (subseq data offset (+ packet-length offset)))
             (packet (make-instance 'encapsulated-packet
                                    :reliability reliability
                                    :has-split has-split
                                    :length packet-length
                                    :identifier-ack identifier-ack
                                    :message-index message-index
                                    :order-index order-index
                                    :order-channel order-channel
                                    :split-count split-count
                                    :split-id split-id
                                    :split-index split-index
                                    :body body)))

        (decode-encapsulated-body (subseq data (+ packet-length offset)) (cons packet acc)))))


(defun split-encapsulated-packet (data &key internal)
  (nreverse (decode-encapsulated-body (subseq data 4) nil :internal internal)))

(defun build-encapsulated-ack-packet (packet)
  (concatenate 'vector
               #( #xC0 )
               #( #x00 #x01 ) ;; number of packets to ACK
               #( #x01 ) ;; aditional packet flag
               (subseq packet 1 4)))

(defun handle-encapsulated-packet (socket src-host src-port packet)
  (let ((replies (apply #'concatenate (cons 'vector (remove nil (mapcar (lambda (part)
                                                                          (let* ((body (packet-body part))
                                                                                 (ep (make-instance (aref raknet-data:*packet-types* (aref body 0)) :body body)))

                                                                            (setf (raknet-data:packet-type ep) (raknet-data:get-byte ep))
                                                                            (raknet-data:decode-data-packet ep)
                                                                            (raknet-data:handle-data-packet ep socket src-host src-port)))
                                                                        (split-encapsulated-packet packet)))))))

    (let ((ack (build-encapsulated-ack-packet packet)))
      (if (= 0 (length replies))
          ack
          (list ack
                (concatenate 'vector
                             #( #x84 )
                             #( #x00 #x00 #x00 )
                             #( #x00 )
                             (raknet-data:short-value (* (length replies) 8))
                             replies))))))


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
(add-packet-handler #xC0 'ignore-packet)

(defclass client-connect-packet (raknet-data:data-packet)
  ((client-id :accessor client-connect-id)
   (session :accessor client-connect-session)
   (unknown :accessor client-connect-unknown)))

(raknet-data:register-packet-type #x09 'client-connect-packet)

(defmethod raknet-data:decode-data-packet ((packet client-connect-packet))
  (setf (client-connect-id packet) (raknet-data:get-long packet))
  (setf (client-connect-session packet) (raknet-data:get-bytes packet 4))
  (setf (client-connect-unknown packet) (raknet-data:get-byte packet))

  packet)

(defmethod raknet-data:handle-data-packet ((packet client-connect-packet) socket src-host src-port)
  (declare (ignore socket src-host))
  (concatenate 'vector
               #( #x10 )
               #( #x04 #x3f #x57 #xfe )
               #( #xcd )
               (raknet-data:short-value src-port)
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
               (client-connect-session packet)
               #( #x00 #x00 #x00 #x00 #x04 #x44 #x0b #xa9 )))

;; CLIENT_HANDSHAKE

(defclass client-handshake (raknet-data:data-packet) ())

(raknet-data:register-packet-type #x13 'client-handshake)
(defmethod raknet-data:decode-data-packet ((packet client-handshake))
  packet)
(defmethod raknet-data:handle-data-packet ((packet client-handshake) socket src-host src-port)
  (declare (ignore packet))
  (if *client-connected-callback* (funcall *client-connected-callback* socket src-host src-port))
  nil)

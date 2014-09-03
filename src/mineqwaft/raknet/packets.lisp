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
(defvar *client-logged-in-callback* nil)

(defun ignore-packet (src-host src-port packet)
  (declare (ignore src-host src-port packet))
  nil)

(defun unknown-packet (src-host src-port packet)
  (format t "Got unknown packet from ~A on port ~A of type ~A~%bytes: ~A~%" src-host src-port (aref packet 0) (hex-dump packet))
  nil)

(defun unknown-encapsulated-packet (src-host src-port packet)
  (format t "Got unknown encapsulated packet from ~A on port ~A of type ~A~%bytes: ~A~%" src-host src-port (aref packet 0) (hex-dump packet))
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
  (format t "Got packet ~A~%" (hex-dump packet))
  (format t "from ~A on port ~A~%" src-host src-port)

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

(defun int-value (int32)
  (list (ash int32 -24)
        (logand (ash int32 -16) #xff)
        (logand (ash int32 -8) #xff)
        (logand int32 #xff)))

(defun get-int (data pos)
  (logior (ash (aref data pos) 24)
          (ash (aref data (+ 1 pos)) 16)
          (ash (aref data (+ 2 pos)) 8)
          (aref data (+ 3 pos))))

(defun float-value (f)
  (int-value (ieee-floats:encode-float32 (float f))))

(defun get-float (data pos)
  (ieee-floats:decode-float32 (get-int data pos)))

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
                           (if *client-added-callback* (funcall *client-added-callback* src-host src-port))

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

(defun build-encapsulated-ack-packet (packet)
  (concatenate 'vector
               #( #xC0 )
               #( #x00 #x01 ) ;; unknown
               #( #x00 ) ;; aditional packet flag
               (subseq packet 1 4)))

(defun split-encapsulated-packet (data)
  (nreverse (decode-encapsulated-body (subseq data 4) nil)))


(defun handle-encapsulated-packet (src-host src-port packet)
  (let ((replies (apply #'concatenate (cons 'vector (remove nil (mapcar (lambda (part)
                                                                          (funcall (aref *encapsulated-packet-handlers* (aref part 0)) src-host src-port part))
                                                                        (split-encapsulated-packet packet)))))))

    (let ((ack (build-encapsulated-ack-packet packet)))
      (if (= 0 (length replies))
          ack
          (list ack
                (concatenate 'vector
                             #( #x84 )
                             #( #x00 #x00 #x00 )
                             #( #x00 )
                             (short-value (* (length replies) 8))
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

(defun handle-login-packet (src-host src-port packet)
  (if *client-logged-in-callback*
      (let* ((name-length (get-short packet 1))
             (name-end (+ 3 name-length))
             (name-bytes (subseq packet 3 name-end)))

        (funcall *client-logged-in-callback* src-host src-port (flexi-streams:octets-to-string name-bytes))))
  (concatenate 'vector
               ;; send a LoginStatus reply saying everything is OK
               ;; TODO: check the client protocol version is good and return the correct results
               #( #x83 #x00 #x00 #x00 #x00 )

               ;; send a StartGame packet
               (concatenate 'vector
                            #( #x87 )
                            (int-value 0) ;; seed
                            (int-value 0) ;; generator
                            (int-value 0) ;; game mode
                            (int-value 100) ;; entity ID
                            (int-value 0) ;; spawn X
                            (int-value 0) ;; spawn Y
                            (int-value 100) ;; sparn Z
                            (float-value 0) ;; X
                            (float-value 0) ;; Y
                            (float-value 101.68)) ;; Z

               ;; send a SetTime Packet
               (concatenate 'vector
                            #( #x86 )
                            (int-value (local-time:timestamp-to-unix (local-time:now)))
                            #( #x80 ))


               ;; send a SetSpawnPosition packet
               (concatenate 'vector
                            #( #xAB )
                            (int-value 0) ;; X
                            (int-value 0) ;; Z
                            #( 100 )) ;; Z

               ;; send a SetHealth packet
               #( #xAA #x14 )))



;; LoginPacket
(add-encapsulated-packet-handler #x82 #'handle-login-packet)

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

(defvar *client-logged-in-callback* nil)

(defclass login-packet (data-packet)
  ((username :accessor client-connect-username)
   (protocol-1 :accessor client-protocol-1)
   (protocol-2 :accessor client-protocol-2)
   (client-id :accessor client-id)
   (login-data :accessor client-login-data)))

(register-packet-type #x82 'login-packet)

(defmethod decode-data-packet((packet login-packet))
  (setf (client-connect-username packet) (get-string packet))
  (setf (client-protocol-1 packet) (get-int packet))
  (setf (client-protocol-2 packet) (get-int packet))
  (setf (client-id packet) (get-int packet))
  (setf (client-login-data packet) (get-string packet))

  packet)

(defmethod handle-data-packet ((packet login-packet) socket src-host src-port)
  (if *client-logged-in-callback*
      (funcall *client-logged-in-callback* socket src-host src-port (client-connect-username packet)))

  (concatenate 'vector
               ;; send a LoginStatus reply saying everything is OK
               ;; TODO: check the client protocol version is good and return the correct results
               #( #x83 #x00 #x00 #x00 #x00 )

               ;; send a StartGame packet
               (concatenate 'vector
                            #( #x87 )
                            (int-value 0) ;; seed
                            (int-value 0) ;; generator
                            (int-value 1) ;; game mode: 0 - survival, 1 - creative
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
                            (int-value 0) ;; DAY TIME
                            #( #x80 ))


               ;; send a SetSpawnPosition packet
               (concatenate 'vector
                            #( #xAB )
                            (int-value 0) ;; X
                            (int-value 0) ;; Z
                            #( 100 )) ;; Y

               ;; send a SetHealth packet
               #( #xAA #x14 )))

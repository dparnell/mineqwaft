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

(defparameter *max-buffer-size* usocket:+max-datagram-packet-size+)

(defvar *send-buffer*
  (make-array *max-buffer-size* :element-type '(unsigned-byte 8) :initial-element 0))

(defvar *receive-buffer*
  (make-array *max-buffer-size* :element-type '(unsigned-byte 8) :initial-element 0))

(defun send-replies (socket src-host src-port replies)
  (if replies
      (let ((reply (car replies)))
        (print (format nil "Sending reply: ~A" reply))
        (print (format nil " to host ~A on port ~A" src-host src-port))

        (replace *send-buffer* reply)
        (usocket:socket-send socket *send-buffer* (length reply) :host src-host :port src-port))
      (send-replies socket src-host src-port (cdr replies))))

(defun process-packet (socket buffer size src-host src-port)
  (let* ((packet (make-array size :element-type '(unsigned-byte 8) :displaced-to buffer))
         (reply (handle-packet src-host src-port packet)))
    (if reply (cond
                ((listp reply) (send-replies socket src-host src-port reply))
                (t (send-replies socket src-host src-port (list reply)))))))

(defun server (socket)
  (multiple-value-bind (buffer size host port)
      (usocket:socket-receive socket *receive-buffer* *max-buffer-size*)
    (process-packet socket buffer size host port))

  (server socket))

(defun serve (interface port)
  (server (usocket:socket-connect nil nil :protocol :datagram
                                  :local-host interface
                                  :local-port port)))

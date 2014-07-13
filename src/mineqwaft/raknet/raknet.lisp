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

(defvar *receive-buffer*
  (make-array *max-buffer-size* :element-type '(unsigned-byte 8) :initial-element 0))

(defun process-packet (socket buffer size src-host src-port)
  (let ((packet (make-array size :element-type '(unsigned-byte 8) :displaced-to buffer)))
    (handle-packet src-host src-port packet)))

(defun server (socket)
  (multiple-value-bind (buffer size host port)
      (usocket:socket-receive socket *receive-buffer* *max-buffer-size*)
    (process-packet socket buffer size host port))

  (server socket))

(defun serve (interface port)
  (server (usocket:socket-connect nil nil :protocol :datagram
                                  :local-host interface
                                  :local-port port)))
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

(in-package :mineqwaft-server)

(defvar *clients* nil)

(defun add-client (client)
  (setf *clients* (cons client *clients*)))

(defun remove-client (client)
  :ok)

(defun find-client (host port)
  (loop for client in *clients*
     when (and (equal (client-host client) host) (equal (client-port client) port))
     return client))

(defun client-added (socket host port)
  (declare (ignore socket))
  (add-client (make-instance 'client :host host :port port)))

(defun client-connected (socket host port)
  (declare (ignore socket))
  (format t "Client connected ~A ~A~%" host port))

(defun client-logged-in (socket host port name)
  (declare (ignore socket))
  (format t "Client logged in ~A ~A - ~A~%" host port name)

  (let ((client (find-client host port)))
    (if client (setf (client-name client) name))))

(defun start (interface port)
  (setf raknet:*client-added-callback* 'client-added)
  (setf raknet:*client-connected-callback* 'client-connected)
  (setf raknet-data:*client-logged-in-callback* 'client-logged-in)

  (format t "Listening on ~A:~D~%" interface port)

  (raknet:serve interface port))

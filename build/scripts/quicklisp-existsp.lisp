;;;; Mineqwaft - A Minecraft server in Lisp
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

(defun loaded-file-directory ()
  (make-pathname
   :host (pathname-host #.(or *compile-file-truename*
                              *load-truename*))
   :directory (pathname-directory #.(or *compile-file-truename*
                                        *load-truename*))))

(defun exit-with-success (success)
  (format t "Quicklisp exists? ~a~%" (if success "yes" "no"))
  #-allegro (quit)
  #+allegro (exit))

#+quicklisp
(exit-with-success t)

(let ((quicklisp-init
       (merge-pathnames "../quicklisp/setup.lisp" (loaded-file-directory))))
  (when (probe-file quicklisp-init)
    (exit-with-success t)))

(exit-with-success nil)

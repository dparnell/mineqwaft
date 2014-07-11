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

;; Load quicklisp
#-quicklisp
(let ((quicklisp-init
       (merge-pathnames "../quicklisp/setup.lisp" (loaded-file-directory))))
  (assert
   (probe-file quicklisp-init)
   ()
   "Failed to find quicklisp. Did you run build/get-dependencies.sh ?")
  (load quicklisp-init))

;; Add any directories in build/libs to the registry
(let ((dir (merge-pathnames "../../" (loaded-file-directory))))
  (pushnew dir asdf:*central-registry* :test #'equal))
(dolist (dir (directory (merge-pathnames #-allegro "../libs/*/"
                                         #+allegro "../libs/"
                                         (loaded-file-directory))
              #+clozure :directories #+clozure t
              #+allegro :directories-are-files #+allegro nil))
  (pushnew dir asdf:*central-registry* :test #'equal))

;; Inject native library paths for cffi
(ql:quickload :cffi)

(pushnew (merge-pathnames #p "../../lib/" (loaded-file-directory))
         cffi:*foreign-library-directories* :test #'equal)

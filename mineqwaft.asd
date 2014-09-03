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

(defpackage :mineqwaft-asd
  (:use :cl :asdf))

(in-package :mineqwaft-asd)

(defsystem mineqwaft
  :name "mineqwaft"
  :author "Daniel Parnell <me@danielparnell.com>"
  :version "0.0"
  :license "MIT"
  :description "A Minecraft server written in lisp"
  :components ((:module :src
                        :components
                        ((:module mineqwaft
                                  :components
                                  ((:module raknet
                                            :components
                                            ((:file "package")
                                             (:file "config")
                                             (:file "utils")
                                             (:file "packets")
                                             (:file "raknet"))
                                            :serial t)
                                   (:module server
                                            :components
                                            ((:file "package")
                                             (:file "client")
                                             (:file "server"))
                                            :serial t)
                                   (:file "package")
                                   (:file "main"))
                                  :serial t))))
  :depends-on (
               :arnesi
               :flexi-streams
               :bordeaux-threads
               :usocket
               :ieee-floats
               :trivial-backtrace
               :local-time))

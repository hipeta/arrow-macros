;;;; cl-thread-macro.asd
;;;;  
;;;; Copyright 2015 hipeta (rhipeta@gmail.com)
;;;;
;;;; This software is released under the MIT License.
;;;; http://opensource.org/licenses/mit-license.php

(in-package :cl-user)
(defpackage cl-thread-macro-asd
  (:use :cl :asdf))
(in-package :cl-thread-macro-asd)

(defsystem cl-thread-macro
  :serial t
  :author "hipeta"
  :license "MIT"
  :description "cl-thread-macro provides clojure-like thread macros and diamond wands."
  :depends-on (:alexandria)
  :components ((:file "cl-thread-macro")))


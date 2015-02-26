;;;; cl-thread-macro-test.asd
;;;;  
;;;; Copyright 2015 hipeta (rhipeta@gmail.com)
;;;;
;;;; This software is released under the MIT License.
;;;; http://opensource.org/licenses/mit-license.php

(in-package :cl-user)
(defpackage cl-thread-macro-test-asd
  (:use :cl :asdf))
(in-package :cl-thread-macro-test-asd)

(defsystem cl-thread-macro-test
  :serial t
  :author "hipeta"
  :license "MIT"
  :description "cl-thread-macro test"
  :depends-on (:fiveam :cl-thread-macro)
  :components ((:file "cl-thread-macro-test")))


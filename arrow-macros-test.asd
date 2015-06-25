;;;; arrow-macros-test.asd
;;;;  
;;;; Copyright 2015 hipeta (rhipeta@gmail.com)
;;;;
;;;; This software is released under the MIT License.
;;;; http://opensource.org/licenses/mit-license.php

(in-package :cl-user)
(defpackage arrow-macros-test-asd
  (:use :cl :asdf))
(in-package :arrow-macros-test-asd)

(defsystem arrow-macros-test
  :serial t
  :author "hipeta"
  :license "MIT"
  :description "arrow-macros test"
  :depends-on (:fiveam :arrow-macros)
  :components ((:file "arrow-macros-test"))
  :perform (test-op (op c)
             (eval (read-from-string "(fiveam::run! 'arrow-macros-test::all)"))))


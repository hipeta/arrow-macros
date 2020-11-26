;;;; arrow-macros.asd
;;;;  
;;;; Copyright 2015 hipeta (rhipeta@gmail.com)
;;;;
;;;; This software is released under the MIT License.
;;;; http://opensource.org/licenses/mit-license.php

(in-package :cl-user)
(defpackage arrow-macros-asd
  (:use :cl :asdf))
(in-package :arrow-macros-asd)

(defsystem arrow-macros
  :serial t
  :author "hipeta"
  :license "MIT"
  :description "arrow-macros provides clojure-like arrow macros and diamond wands."
  :depends-on (:alexandria)
  :components ((:file "arrow-macros"))
  :in-order-to ((test-op (test-op :arrow-macros-test))))

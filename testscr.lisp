
(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :fiveam))

(defun test (sys)
  (handler-case
      (progn
        (ql:quickload sys)
        (asdf:load-system sys)
        (eval (read-from-string "(fiveam:run! 'arrow-macros-test::all)")))
    (serious-condition (c)
      (describe c)
      (uiop:quit 2))))

(uiop:quit (if (every #'fiveam::TEST-PASSED-P
                      (test :arrow-macros))
               0 1))

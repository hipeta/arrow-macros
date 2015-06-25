
(in-package :cl-user)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (ql:quickload :arrow-macros-test))

(uiop:quit (if (every #'fiveam::TEST-PASSED-P
                      (handler-case
                          (fiveam:run! 'arrow-macros-test::all)
                        (serious-condition (c)
                          (describe c)
                          (uiop:quit 2))))
               0 1))

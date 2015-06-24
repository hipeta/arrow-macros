;;;; arrow-macros.lisp
;;;;  
;;;; Copyright 2015 hipeta (rhipeta@gmail.com)
;;;;
;;;; This software is released under the MIT License.
;;;; http://opensource.org/licenses/mit-license.php

(in-package :cl-user)
(defpackage arrow-macros
  (:use :cl)
  (:export :->
           :->>
           :some->
           :some->>
           :as->
           :cond->
           :cond->>
           :-<>
           :-<>>
           :some-<>
           :some-<>>
           :<>))
(in-package :arrow-macros)

(defun arrow-macro (init exps &optional >>-p some-p)
  (let ((exps (mapcar (lambda (exp)
                        (cond ((symbolp exp) `(,exp))
                              ((and (typep exp 'cons) (eq 'function (car exp)))
                               (if >>-p
                                   `(funcall (function ,(cadr exp)))
                                   `(->> (funcall (function ,(cadr exp))))))
                              ((and (typep exp 'cons) (eq 'lambda (car exp)))
                               (if >>-p
                                   `(funcall ,exp)
                                   `(->> (funcall ,exp))))
                              (t exp)))
                      exps)))
    (cond (some-p
           (let ((gblock (gensym)))
             `(block ,gblock
                ,(cadr
                  (let ((init `(or ,init (return-from ,gblock nil))))
                    (if >>-p
                        (reduce (lambda (e1 e2)
                                  `(or ,(append e2 (cons e1 nil)) (return-from ,gblock nil)))
                                (cons init exps))
                        (reduce (lambda (e1 e2)
                                  `(or (,(car e2) ,e1 ,@(cdr e2)) (return-from ,gblock nil)))
                                (cons init exps))))))))
          (>>-p (reduce (lambda (e1 e2) (append e2 (cons e1 nil))) (cons init exps)))
          (t (reduce (lambda (e1 e2) `(,(car e2) ,e1 ,@(cdr e2))) (cons init exps))))))

(defmacro -> (init &body exps) (arrow-macro init exps))
(defmacro ->> (init &body exps) (arrow-macro init exps t))
(defmacro some-> (init &body exps) (arrow-macro init exps nil t))
(defmacro some->> (init &body exps) (arrow-macro init exps t t))

(defmacro as-> (init var &body exps)
  `(let ((,var ,init))
     ,@(loop for (exp next-exp) on exps
          collect (if next-exp `(setf ,var ,exp) exp))))

(defun cond-arrow-macro (init exps &optional >>-p)
  (let ((gvar (gensym)) (arrow (if >>-p '->> '->)))
    `(-> ,init
       ,@(loop for (pred form) on exps by #'cddr
            collect `(lambda (,gvar) (if ,pred (,arrow ,gvar ,form) ,gvar))))))

(defmacro cond-> (init &body exps) (cond-arrow-macro init exps))
(defmacro cond->> (init &body exps) (cond-arrow-macro init exps t))

;; (define-symbol-macro <> (error "do not use <> outside the scope of diamond-wand macros!"))

(defun has-diamond% (form env)
  (handler-bind ((hu.dwim.walker:walker-warning #'muffle-warning))
    (let* ((walked (hu.dwim.walker:walk-form
                    form
                    :environment (hu.dwim.walker:make-walk-environment env)))
           (refs (hu.dwim.walker:collect-variable-references
                  walked
                  :type 'hu.dwim.walker:free-variable-reference-form))
           (refs (append refs
                         (hu.dwim.walker:collect-variable-references
                          walked
                          :type 'hu.dwim.walker:unwalked-lexical-variable-reference-form))))
      (find '<> (mapcar #'hu.dwim.walker:name-of refs)))))

(defun has-diamond (form env)
  "Return true when the form uses <> as a variable reference."
  ;; Note that simple tree parsing does not work for the cases like
  ;; (let ((<> ...)) ...)
  (handler-case (has-diamond% form env)
    #+sbcl(sb-kernel::arg-count-error () nil)
    #-sbcl(error () nil)))

#|

I strongly object to the inappropriate/implicit use of intern, which
results in unhealthy macrology.

Imagine a case where a diamond-wand is used in the expansion of another
macro `mymacro', written by user1. He might export the macro as part of a library.

 (in-package :a)
 (use-package :arrow-macros)
 (defmacro mymacro (&body body)
   `(-<> 42
         (process-it <>) ;; interned, a::<>
         ,@body))
 (export 'mymacro)

Now, another lisper called user2 may use this macro
without knowing that the diamond-wand is used in it.

 (in-package :b)
 (use-package :a)

 (defvar <> 'something)

 (mymacro (print <>))

And the result is unexpected!

 -->
 (-<> 42
      (process-it a::<>)
      (print b::<>))

It will be interpreted as (process-it a::<> b::<>) and signals an
symbol-unbound error on a::<>.

|#


(defun diamond-wand (init exps env &optional spear-p some-p)
  (let ((gblock (gensym)))
    (if some-p
        `(block ,gblock
           (let ((<> (or ,init (return-from ,gblock nil))))
               ,@(loop for (exp next-exp) on exps collect
                 (let ((exp (cond ((has-diamond exp env) exp)
                                  (spear-p `(->> <> ,exp))
                                  (t `(-> <> ,exp)))))
                   (if next-exp
                       `(setf <> (or ,exp (return-from ,gblock nil)))
                            exp)))))
        `(let ((<> ,init))
           ,@(loop for (exp next-exp) on exps collect
               (let ((exp (cond ((has-diamond exp env) exp)
                                (spear-p `(->> <> ,exp))
                                (t `(-> <> ,exp)))))
                 (if next-exp `(setf <> ,exp) exp)))))))

(defmacro -<> (init &body exps &environment env) (diamond-wand init exps env))
(defmacro -<>> (init &body exps &environment env) (diamond-wand init exps env t))
(defmacro some-<> (init &body exps &environment env) (diamond-wand init exps env nil t))
(defmacro some-<>> (init &body exps &environment env) (diamond-wand init exps env t t))

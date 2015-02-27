;;;; arrow-macros.lisp
;;;;  
;;;; Copyright 2015 hipeta (rhipeta@gmail.com)
;;;;
;;;; This software is released under the MIT License.
;;;; http://opensource.org/licenses/mit-license.php

(in-package :cl-user)
(defpackage arrow-macros
  (:use :cl)
  (:import-from :alexandria :flatten)
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
           :some-<>>))
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

(defun diamond-wand (init exps &optional spear-p some-p)
  (symbol-macrolet ((<> (intern "<>")))
    (let ((gblock (gensym)))
      `(block ,gblock
         (let ((,<> ,(if some-p `(or ,init (return-from ,gblock nil)) init)))
           ,@(loop for (exp next-exp) on exps collect
                  (let* ((exp (cond ((member <> (flatten exp)) exp)
                                    (spear-p `(->> ,<> ,exp))
                                    (t `(-> ,<> ,exp))))
                         (exp (if (and next-exp some-p)
                                  `(or ,exp (return-from ,gblock nil))
                                  exp)))
                    (if next-exp `(setf ,<> ,exp) exp))))))))

(defmacro -<> (init &body exps) (diamond-wand init exps))
(defmacro -<>> (init &body exps) (diamond-wand init exps t))
(defmacro some-<> (init &body exps) (diamond-wand init exps nil t))
(defmacro some-<>> (init &body exps) (diamond-wand init exps t t))


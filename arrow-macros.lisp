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
           :<>
           :<!>))
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
     ,var
     ,@(loop for (exp next-exp) on exps
          collect (if next-exp `(setf ,var ,exp) exp))))

(defun cond-arrow-macro (init exps &optional >>-p)
  (let ((gvar (gensym)) (arrow (if >>-p '->> '->)))
    `(-> ,init
       ,@(loop for (pred form) on exps by #'cddr
            collect `(lambda (,gvar) (if ,pred (,arrow ,gvar ,form) ,gvar))))))

(defmacro cond-> (init &body exps) (cond-arrow-macro init exps))
(defmacro cond->> (init &body exps) (cond-arrow-macro init exps t))

(defparameter *diamond-wands* '(-<> -<>> some-<> some-<>>))

(defun diamond-wand-symbol-p (sym) (member sym *diamond-wands* :test #'eq))

(defun has-diamond (exp)
  (labels ((rec (exp)
             (cond ((eq exp '<>) t)

                   ; inner diamond wand can refer to outer <> symbol only in initial form
                   ((and (listp exp) (diamond-wand-symbol-p (car exp)))
                    (rec (cadr exp)))
                   
                   ((listp exp) (mapcar #'rec exp))
                   (t nil))))
    (let ((fexp (alexandria:flatten (rec exp))))
      (and fexp (reduce (lambda (a b) (or a b)) fexp)))))

(defun replace-diamond (exp diamond-exp)
  (cond ((eq exp '<>) diamond-exp)
        ((and (listp exp) (diamond-wand-symbol-p (car exp)))
         (let ((init (replace-diamond (cadr exp) diamond-exp)))
           (case (car exp)
             (-<>      (diamond-wand init (cddr exp)))
             (-<>>     (diamond-wand init (cddr exp) t))
             (some-<>  (diamond-wand init (cddr exp) nil t))
             (some-<>> (diamond-wand init (cddr exp) t t)))))
        ((listp exp) (mapcar (lambda (x) (replace-diamond x diamond-exp)) exp))
        (t exp)))

(defun diamond-wand% (diamond-exp exps some-p)
  (let ((gblock (gensym)))
    (labels ((rec (diamond-exp exps)
               (let ((diamond-exp (if some-p
                                      `(or ,diamond-exp (return-from ,gblock nil))
                                      diamond-exp)))
                 (cond ((eq (car exps) '<!>)
                        (let ((gvar (gensym)))
                          `(let ((,gvar ,diamond-exp))
                             ,(rec gvar (cdr exps)))))
                       (exps (rec (replace-diamond (car exps) diamond-exp) (cdr exps)))
                       (t (if some-p
                              (cadr diamond-exp) ; outermost parenthesis shouldn't be sandwiched by `(or ~~ (return-from ,gblock nil))
                              diamond-exp))))))
      (if some-p
          `(block ,gblock ,(rec diamond-exp exps))
          (rec diamond-exp exps)))))

(defun diamond-wand (init exps &optional >>-p some-p)
  (let* (; preprocessing for lambda, function, <!>, one symbol expressions
         (exps (loop for exp in exps collect (cond ((and (symbolp exp)
                                                         (not (eq exp '<!>)) `(,exp)))
                                                   ((and (consp exp) (eq 'function (car exp)))
                                                    `(funcall ,exp <>))
                                                   ((and (consp exp) (eq 'lambda (car exp)))
                                                    `(funcall ,exp <>))
                                                   (t exp))))
         ; supplement expressions with diamond symbols
         (exps (loop for exp in exps collect (cond ((eq '<!> exp) exp)
                                                   ((has-diamond exp) exp)
                                                   (>>-p
                                                    `(,(car exp) ,@(cdr exp) <>))
                                                   (t
                                                    `(,(car exp) <> ,@(cdr exp)))))))
    (diamond-wand% init exps some-p)))

(defmacro -<> (init &body exps) (diamond-wand init exps))
(defmacro -<>> (init &body exps) (diamond-wand init exps t))
(defmacro some-<> (init &body exps) (diamond-wand init exps nil t))
(defmacro some-<>> (init &body exps) (diamond-wand init exps t t))


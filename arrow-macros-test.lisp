;;;; arrow-macros-test.lisp
;;;;  
;;;; Copyright 2015 hipeta (rhipeta@gmail.com)
;;;;
;;;; This software is released under the MIT License.
;;;; http://opensource.org/licenses/mit-license.php

(in-package :cl-user)
(defpackage arrow-macros-test
  (:use :cl :fiveam :arrow-macros))
(in-package :arrow-macros-test)

(def-suite all)
(in-suite all)

(test ->-test
  (is (= 3 (-> 3)))
  (is (= (-> 1 (+ 2 3) #'1+ 1+ (lambda (x) (+ x 1)) (1+)) 10))
  (is (equal (-> 1 (cons 2) (cons 3)) (cons (cons 1 2) 3)))
  (is (multiple-value-bind (x y) (-> 3 (1+) (values 2))
        (= (+ x y) 6))))

(test ->>-test
  (is (= 3 (->> 3)))
  (is (= (->> 1 (+ 2 3) #'1+ 1+ (lambda (x) (+ x 1)) (1+)) 10))
  (is (equal (->> 1 (cons 2) (cons 3)) (cons 3 (cons 2 1))))
  (is (multiple-value-bind (x y) (->> 3 (1+) (values 2))
        (= (+ x y) 6))))

(test some->-test
  (is (= 3 (some-> 3)))
  (is (= (some-> 1 (+ 2 3) #'1+ 1+ (lambda (x) (+ x 1)) (1+)) 10))
  (is (null (some-> nil (cons 3))))
  (is (null (some-> 3 (cons 4) not not)))
  (is (equal (some-> 1 (cons 2) (cons 3)) (cons (cons 1 2) 3)))
  (is (multiple-value-bind (x y) (some-> 3 (1+) (values 2))
        (= (+ x y) 6))))

(test some->>-test
  (is (= 3 (some->> 3)))
  (is (= (some->> 1 (+ 2 3) #'1+ 1+ (lambda (x) (+ x 1)) (1+)) 10))
  (is (null (some->> nil (cons 3))))
  (is (null (some->> 3 (cons 4) not not)))
  (is (equal (some->> 1 (cons 2) (cons 3)) (cons 3 (cons 2 1))))
  (is (multiple-value-bind (x y) (some->> 3 (1+) (values 2))
        (= (+ x y) 6))))

(test as->-test
  (is (= 3 (as-> 3 x)))
  (is (= (as-> 4 x (1+ x) (+ x x)) 10))
  (is (multiple-value-bind (x y) (as-> 3 x (1+ x) (values x 2))
        (= (+ x y) 6))))

(test cond->-test
  (is (= 3 (cond-> 3)))
  (is (= (let ((x '(2 4)))
           (cond-> 0
             (member 1 x) 1+
             (member 2 x) (+ 2)
             (member 3 x) (+ 3)
             (member 4 x) (+ 4)))
         6))
  (is (equal (cond-> nil
               t (cons 1)
               nil (cons 2)
               t (cons 3))
             (cons (cons nil 1) 3)))
  (is (= (cond-> 0 t 1+ t #'1+ t (1+) t (lambda (x) (+ 1 x))) 4))
  (is (multiple-value-bind (x y) (cond-> 3 t 1+ t (values 2))
        (= (+ x y) 6))))

(test cond->>-test
  (is (= 3 (cond->> 3)))
  (is (= (let ((x '(2 4)))
           (cond->> 0
             (member 1 x) 1+
             (member 2 x) (+ 2)
             (member 3 x) (+ 3)
             (member 4 x) (+ 4)))
         6))
  (is (equal (cond->> nil
               t (cons 1)
               nil (cons 2)
               t (cons 3))
             (cons 3 (cons 1 nil))))
  (is (= (cond->> 0 t 1+ t #'1+ t (1+) t (lambda (x) (+ 1 x))) 4))
  (is (multiple-value-bind (x y) (cond->> 3 t 1+ t (values 2))
        (= (+ x y) 6))))

(test -<>-test
  (is (= 3 (-<> 3)))
  (is (= (-<> 0 1+ #'1+ (1+) (lambda (x) (+ 1 x)) (1+ <>) (+ <> <>)) 10))
  (is (equal (-<> nil (cons 3) (cons 4) (cons <> <>))
             (cons (cons (cons nil 3) 4) (cons (cons nil 3) 4))))
  (is (multiple-value-bind (x y) (-<> 0 (+ 2) 1+ #'1+ (values <> 2))
        (= (+ x y) 6))))

(test -<>>-test
  (is (= 3 (-<>> 3)))
  (is (= (-<>> 0 1+ #'1+ (1+) (lambda (x) (+ 1 x)) (1+ <>) (+ <> <>)) 10))
  (is (equal (-<>> nil (cons 3) (cons 4) (cons <> <>))
             (cons (cons 4 (cons 3 nil)) (cons 4 (cons 3 nil)))))
  (is (multiple-value-bind (x y) (-<>> 0 (+ 2) 1+ #'1+ (values <> 2))
        (= (+ x y) 6))))

(test some-<>-test
  (is (= 3 (some-<> 3)))
  (is (= (some-<> 0 1+ #'1+ (1+) (lambda (x) (+ 1 x)) (1+ <>) (+ <> <>)) 10))
  (is (equal (some-<> t (cons 3) (cons 4) (cons <> <>))
             (cons (cons (cons t 3) 4) (cons (cons t 3) 4))))
  (is (multiple-value-bind (x y) (some-<> 0 (+ 2) 1+ #'1+ (values <> 2))
        (= (+ x y) 6)))
  (is (null (some-<> nil (cons 3))))
  (is (null (some-<> 3 (cons 4) not not))))

(test some-<>>-test
  (is (= 3 (some-<>> 3)))
  (is (= (some-<>> 0 1+ #'1+ (1+) (lambda (x) (+ 1 x)) (1+ <>) (+ <> <>)) 10))
  (is (equal (some-<>> t (cons 3) (cons 4) (cons <> <>))
             (cons (cons 4 (cons 3 t)) (cons 4 (cons 3 t)))))
  (is (multiple-value-bind (x y) (some-<>> 0 (+ 2) 1+ #'1+ (values <> 2))
        (= (+ x y) 6)))
  (is (null (some-<>> nil (cons 3))))
  (is (null (some-<>> 3 (cons 4) not not))))

; obsolte
;(test sbcl-backquote-test
;  (is (equal (-<> 3 (list 3 `(,<> ,<>) 4)) '(3 (3 3) 4))))

(test <>-symbol-interpretation-test
  (is (equal (-<>> (list 1 2 3 4 5)
               (mapcar (lambda (x) (-<> x (+ <> <>)))))
             (-<>> (list 1 2 3 4 5)
               (mapcar (lambda (x) (-<> x (+ <> <>))) <>)))))

(test diamond-wand-includes-macro-exps-test
  (is (equal (let (p) (-<> 3 (push p))) '(3)))
  (is (equal (let (p) (-<> 3 (push p) (push p) (push p)
                           (-<>> <> (mapcar (lambda (x) (-<> x (listp <>)))))))
             (list t t nil))))

(test diamond-wand-implicit-evaluation-test
  (is (equal (macroexpand-1 '(-<> foo (defun <> (bar (1+ bar)))))
             '(defun foo (bar (1+ bar)))))
  (is (equal (macroexpand-1 '(-<> foo (defun (bar (1+ bar)))))
             '(defun foo (bar (1+ bar))))))

(test nested-diamond-wand-test1
  (is (= (-<> 1 (+ <> <>) (-<> (+ <> <> <>))) 6))
  (is (equal (macroexpand-1 '(-<> 1 (+ <> <>) (-<> (+ <> <> <>))))
             '(+ (+ 1 1) (+ 1 1) (+ 1 1)))))

(test nested-diamond-wand-test2
  (is (= (-<> 1 1+ (1+) #'1+ (lambda (x) (1+ x))
           (+ <> (-<> <> 1+ (1+) #'1+ (lambda (x) (1+ x)))))
         14)))

(test nested-diamond-wand-test3
  (is (= (-<> 1 (+ 4)
           (+ (-<> <> (+ <> <>))))
         10)))

(test nested-diamond-wand-test4
  (is (= (-<> 1 (+ 4)
           (+ (-<> <> (-<> <> (+ <> <>)))))
         10)))

(test nested-diamond-wand-test5
  (is (equal (macroexpand-1 '(-<> 1 (+ 2 (-<> 3))))
             '(+ 1 2 3)))
  (is (= (-<> 1 (+ 2 (-<> 3))) 6)))

(test nested-diamond-wand-test6
  (is (equal (-<>> (list 1 2 3)
               (mapcar #'1+)
               (-<> <> (sort #'>))
               (mapcar #'1+))
             (list 5 4 3))))

(test nested-diamond-wand-test7
  (is (null (some-<> nil)))
  (is (= (some-<> 3 1+ (-<> (+ 4))) 8))
  (let ((x 3))
    (is (null (some-<> (list 1 2 3)
                (-<>> <> (reduce #'+))
                (= 5)
                (setf x <>)
                x)))))

(test nested-diamond-wand-scope-test
  (is (= 5 (-<> 1 (-<> 1+ (-<> 1+ (-<> 1+ (-<> 1+)))))))
  
  (is (equal (macroexpand-1 '(-<> 1
                              (* 2 (-<> (+ 1)))))
             '(* 1 2 (+ 1))))
  (is (= 2 (-<> 1 (* 2 (-<> (+ 1))))))

  (is (equal (macroexpand-1 '(-<> 1
                              (* 2 (-<> (+ 1 <>)))))
             '(* 2 (+ 1 1))))
  (is (= 4 (-<> 1 (* 2 (-<> (+ 1 <>))))))

  (is (equal (macroexpand-1 '(-<> 1
                              (* 2 (-<> (+ 1) (+ <>)))))
             '(* 1 2 (+ (+ 1)))))
  (is (= 2 (-<> 1 (* 2 (-<> (+ 1) (+ <>)))))))

(test diamond-wand-side-effect-test
  (let ((x 0))
    (labels ((f (a b) (incf x a) (incf x b)))
      (is (= (-<> 1 (f <> <>) (+ x <> <>))         ; => (+ x (f 1 1) (f 1 1)) where x = 0
             6))
      (is (= x 4))))
  (let ((x 0))
    (labels ((f (a b) (incf x a) (incf x b)))
      (is (= (-<> 1 (f <> <>) <!> (+ x <> <>))     ; => (+ x 2 2) where x = 2
             6))
      (is (= x 2))))
  (let ((x 0))
    (is (equal (list (-<> x
                       incf
                       (+ <> <>)
                       (+ <> <>))
                     x)
               (list 10 4))))
  (let ((x 0))
    (is (equal (list (-<> x
                       incf <!>
                       (+ <> <>)
                       (+ <> <>))
                     x)
               (list 4 1)))))

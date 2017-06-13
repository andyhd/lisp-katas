(defpackage :evolution
  (:use :cl :fiveam))

(in-package :evolution)

(defun random-char ()
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZ "))
    (char chars (random (length chars)))))

(defun random-string (len)
  (let ((str (make-list len)))
    (mapcar (lambda (x) x (random-char)) str)))

(defun hamming-distance (a b)
  (apply '+ (mapcar (lambda (i j) (if (eq i j) 0 1)) a b)))

(defun mutate (str)
  (mapcar (lambda (c) (if (< 95 (random 101)) (random-char) c)) str))

(defun generation (parent)
  (mapcar (lambda (s) (mutate s)) (make-list 100 :initial-element parent)))

(defun evolve (str i fitness)
  (print (format nil "Generation ~A: ~A (~A)" i (format nil "~{~A~}" str) (funcall fitness str)))
  (cond
    ((= 0 (funcall fitness str)) str)
    (t (evolve (reduce (lambda (a b) (if (< (funcall fitness a) (funcall fitness b)) a b)) (generation str)) (+ 1 i) fitness))))

(defun evolution (target)
  (flet ((fitness (s) (hamming-distance s (coerce target 'list))))
    (evolve (random-string (length target)) 0 #'fitness)))


(def-suite test-evolution)

(in-suite test-evolution)

(test hamming-distance
  (let ((a (coerce "hello" 'list)))
    (is (= 0 (hamming-distance a a)))
    (is (= 1 (hamming-distance a (coerce "hallo" 'list))))
    (is (= 1 (hamming-distance a (coerce "jello" 'list))))
    (is (= 5 (hamming-distance a (coerce "asdfg" 'list))))))

(run! 'test-evolution)

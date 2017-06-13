(defpackage :life
  (:use :cl :fiveam))

(in-package :life)

(defun count-live-neighbours (world x y)
  (let ((num-live 0))
    (dolist (offset-y '(-1 0 1))
      (dolist (offset-x '(-1 0 1))
        (destructuring-bind (max-y max-x) (array-dimensions world)
          (incf num-live
                (cond
                  ((not (and (<= 0 (+ x offset-x) (- max-x 1))
                             (<= 0 (+ y offset-y) (- max-y 1)))) 0)
                  ((= 0 offset-x offset-y) 0)
                  (t (aref world (+ y offset-y) (+ x offset-x))))))))
    num-live))

(defun iter-cells (world fn)
  (destructuring-bind (w h) (array-dimensions world)
    (dotimes (y h)
      (dotimes (x w)
        (apply fn (list x y))))))

(defun display (world)
  (iter-cells world
              (lambda (x y)
                (and (= 0 x) (princ #\newline))
                (princ (aref world y x))))
  (princ #\newline)
  nil)

(defun next-gen (world)
  (let ((new-world (make-array (array-dimensions world))))
    (iter-cells world
                (lambda (x y)
                  (let ((num-neighbours (count-live-neighbours world x y))
                        (live (= 1 (aref world y x))))
                    (setf (aref new-world y x)
                          (cond
                            ((and live (< num-neighbours 2)) 0)
                            ((and live (> num-neighbours 3)) 0)
                            (live 1)
                            ((and (not live) (= 3 num-neighbours)) 1)
                            (t 0))))))
    new-world))


(defun all-dead (world)
  (reduce (lambda (a b) (and a (= 0 b)))
          (make-array (array-total-size world) :displaced-to world)
          :initial-value T))

(defun life (world)
  (cond
    ((null world) nil)
    ((all-dead world) (print "All cells died"))
    (t (progn
         (display world)
         (read-char)
         (setf world (next-gen world))
         (life world)))))

(def-suite conway-life :description "Test game of life")

(in-suite conway-life)

(defun set-live (world coords)
  (mapcar (lambda (coord)
            (setf (apply #'aref world coord) 1))
          coords)
  )

(test live-cell-with-fewer-than-two-neighbours-dies

  (let ((world (make-array '(5 5))))
    (set-live world '((2 2)))
    (setf world (next-gen world))
    (is (= (aref world 2 2) 0)))

  (let ((world (make-array '(5 5))))
    (set-live world '((2 2) (3 2)))
    (setf world (next-gen world))
    (is (= (aref world 2 2) 0))
    (is (= (aref world 3 2) 0)))
  )

(test live-cell-with-more-than-three-live-neighbours-dies

  (let ((world (make-array '(5 5))))
    (set-live world '((1 2) (2 1) (2 2) (2 3) (3 2)))
    (setf world (next-gen world))
    (is (= (aref world 2 2) 0)))
  )

(test live-cell-with-two-or-three-live-neighbours-survives

  (let ((world (make-array '(5 5))))
    (set-live world '((1 1) (1 2) (2 1) (2 2)))
    (setf world (next-gen world))
    (is (= 1 (aref world 2 2))))
  )

(test dead-cell-with-three-live-neighbours-comes-alive

  (let ((world (make-array '(5 5))))
    (set-live world '((1 2) (2 1) (2 3)))
    (setf world (next-gen world))
    (is (= 1 (aref world 2 2))))
  )

(run! 'conway-life)

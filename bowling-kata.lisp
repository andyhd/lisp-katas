(defpackage :bowling-game
  (:use #:cl #:fiveam)
  (:export #:main))

(in-package :bowling-game)

(defclass game ()
  ((rolls :initform () :accessor rolls)
   (current-roll :initform 0 :accessor current-roll)))

(defmethod roll ((game game) pins)
  (setf (rolls game) (append (rolls game) (list pins))))

(defun frames (rolls)
  (cond
    ((null rolls) nil)
    ((null (cdr rolls)) (list rolls))
    ((null (cddr rolls)) (list rolls))
    ((null (cdddr rolls)) (list rolls))
    ((= 10 (car rolls)) (append (list (list (car rolls))) (frames (cdr rolls))))
    (t (append (list (list (car rolls) (cadr rolls))) (frames (cddr rolls))))))

(defun is-strike (frame)
  (= 10 (car frame)))

(defun is-spare (frame)
  (= 10 (apply '+ frame)))

(defun strike-bonus (frames)
  (if (null (cadar frames))
   (+ (caadr frames)
      (if (null (cadadr frames))
        (caaddr frames)
        (cadadr frames)))
   (+ (cadar frames) (cadadr frames))))

(defun spare-bonus (frames)
  (caadr frames))

(defun frame-scores (frames)
  (if (null frames)
    0
    (+ (cond
         ((is-strike (car frames)) (+ 10 (strike-bonus frames)))
         ((is-spare (car frames)) (+ 10 (spare-bonus frames)))
         (t (apply '+ (car frames))))
       (frame-scores (cdr frames)))))

(defmethod score ((game game))
  (frame-scores (frames (rolls game))))

(def-suite bowling-game-test :description "Test the bowling game class")

(in-suite bowling-game-test)

(defun roll-many (game rolls pins)
  (dotimes (i rolls)
    (roll game pins)))

(test gutter-game
  (let ((game (make-instance 'game)))
    (roll-many game 20 0)
    (is (= 0 (score game)))))

(test all-ones
  (let ((game (make-instance 'game)))
    (roll-many game 20 1)
    (is (= 20 (score game)))))

(defun roll-spare (game)
  (roll game 5)
  (roll game 5))

(test one-spare
  (let ((game (make-instance 'game)))
    (roll-spare game)
    (roll game 3)
    (roll-many game 17 0)
    (is (= 16 (score game)))))

(defun roll-strike (game)
  (roll game 10))

(test one-strike
  (let ((game (make-instance 'game)))
    (roll-strike game)
    (roll game 3)
    (roll game 4)
    (roll-many game 16 0)
    (is (= 24 (score game)))))

(test perfect-game
  (let ((game (make-instance 'game)))
    (roll-many game 12 10)
    (is (= 300 (score game)))))

(run! 'bowling-game-test)

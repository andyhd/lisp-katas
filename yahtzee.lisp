(defpackage :yahtzee
  (:use :cl :fiveam))

(in-package :yahtzee)

(defun chance-score (dice)
  (apply #'+ dice))

(defun yahtzee-score (dice)
  (if (apply #'= dice)
    50
    0))

(defun number-score (num dice)
  (apply #'+ (mapcar (lambda (x) (if (= num x) x 0)) dice)))

(defun groups (dice)
  (reduce (lambda (a b)
            (cond
              ((and (not (listp a)) (= a b)) `((,b ,a)))
              ((not (listp a)) `((,b) (,a)))
              ((= (caar a) b)
               (progn
                 (setf (car a) (append (car a) (list b)))
                 a))
              (t (append `((,b)) a))))
          (sort dice #'<))
  )

(defun n-groups (n dice)
  (reduce (lambda (a b)
            (append a (cond
              ((= n (length b)) (list b))
              (t nil)
              )))
          (groups dice)
          :initial-value nil)
  )

(defun pairs (dice)
  (n-groups 2 dice))

(defun max-pair (dice)
  (let ((pairs (pairs dice)))
    (if (null pairs)
      '(0 0)
      (car pairs)))
  )

(defun pair-score (dice)
  (apply #'+ (max-pair dice)))

(defun two-pairs-score (dice)
  (let ((pairs (pairs dice)))
    (if (= 2 (length pairs))
      (apply #'+ (mapcar (lambda (pair) (apply #'+ pair)) pairs))
      0))
  )

(defun triples (dice)
  (n-groups 3 dice))

(defun three-of-a-kind-score (dice)
  (let ((triples (triples dice)))
    (if triples
      (apply #'+ (car triples))
      0))
  )

(defun fours (dice)
  (n-groups 4 dice))

(defun four-of-a-kind-score (dice)
  (let ((fours (fours dice)))
    (if fours
      (apply #'+ (car fours))
      0))
  )

(defun small-straight-score (dice)
  (if (equal (sort dice #'<) '(1 2 3 4 5))
    15
    0))

(defun large-straight-score (dice)
  (if (equal (sort dice #'<) '(2 3 4 5 6))
    20
    0))

(defun full-house-score (dice)
  (let ((groups (groups dice)))
    (if (= 2 (length groups))
      (apply #'+ (mapcar (lambda (group) (apply #'+ group)) groups))
      0)))

(def-suite yahtzee :description "Test the Yahtzee score calculator")

(in-suite yahtzee)

(test chance
  (is (= 14 (chance-score '(1 1 3 3 6))))
  (is (= 21 (chance-score '(4 5 5 6 1)))))

(test yahtzee-score
  (is (= 50 (yahtzee-score '(1 1 1 1 1))))
  (is (= 0 (yahtzee-score '(1 1 1 2 1)))))

(test number-score
  (is (= 8 (number-score 4 '(1 1 2 4 4))))
  (is (= 4 (number-score 2 '(2 3 2 5 1))))
  (is (= 0 (number-score 1 '(3 3 3 4 5)))))

(test pair-score
  (is (= 8 (pair-score '(3 3 3 4 4))))
  (is (= 12 (pair-score '(1 1 6 2 6))))
  (is (= 0 (pair-score '(3 3 3 4 1))))
  (is (= 0 (pair-score '(3 3 3 3 1)))))

(test two-pairs-score
  (is (= 8 (two-pairs-score '(1 1 2 3 3))))
  (is (= 14 (two-pairs-score '(3 3 2 4 4))))
  (is (= 0 (two-pairs-score '(1 1 2 3 4))))
  (is (= 0 (two-pairs-score '(1 1 2 2 2)))))

(test three-of-a-kind
  (is (= 9 (three-of-a-kind-score '(3 3 3 4 5))))
  (is (= 0 (three-of-a-kind-score '(3 3 4 5 6))))
  (is (= 0 (three-of-a-kind-score '(3 3 3 3 1)))))

(test four-of-a-kind
  (is (= 8 (four-of-a-kind-score '(2 2 2 2 5))))
  (is (= 0 (four-of-a-kind-score '(2 2 2 5 5))))
  (is (= 0 (four-of-a-kind-score '(2 2 2 2 2)))))

(test small-straight
  (is (= 15 (small-straight-score '(1 2 3 4 5)))))

(test large-straight
  (is (= 20 (large-straight-score '(2 3 4 5 6)))))

(test full-house
  (is (= 8 (full-house-score '(1 1 2 2 2))))
  (is (= 0 (full-house-score '(2 2 3 3 4))))
  (is (= 0 (full-house-score '(4 4 4 4 4)))))

(run! 'yahtzee)

(defpackage :poker-hands
  (:use :cl :fiveam))

(in-package :poker-hands)

(defun card-value (card)
  (let ((value (car card)))
    (cond
      ((integerp value) value)
      ((eq 'J value) 11)
      ((eq 'Q value) 12)
      ((eq 'K value) 13)
      ((eq 'A value) 14)))
  )

(defun rank-high-card (hand)
  (apply #'max (mapcar #'card-value hand)))

(defun sequential-p (numbers)
  (cond
    ((null numbers) t)
    ((null (cdr numbers)) t)
    ((not (= 1 (- (car numbers) (cadr numbers)))) nil)
    (t (sequential-p (cdr numbers))))
  )

(defun straight-p (hand)
  (let ((card-values (sort (mapcar #'card-value hand) #'>)))
    (cond
      ((sequential-p card-values) t)
      ; Aces can be low
      ((= 14 (car card-values))
       (progn
         (setf card-values (append (cdr card-values) '(1)))
         (sequential-p card-values)))
      (t nil)))
  )

(defun suit-value (card)
  (let ((suit (cadr card)))
    (cond
      ((eq 'C suit) 1)
      ((eq 'D suit) 2)
      ((eq 'H suit) 3)
      ((eq 'S suit) 4)))
  )

(defun flush-p (hand)
  (apply #'= (mapcar #'suit-value hand)))

(defun groups (cards)
  (cond

    ; () -> ()
    ((null cards) nil)

    ; ((6 H)) -> (((6 H)))
    ((null (cdr cards)) (list cards))

    ; ((6 H) (6 C)) -> (((6 H) (6 C)))
    ((= (card-value (car cards)) (card-value (cadr cards)))
     (let ((rest (groups (cdr cards))))
       (append `(,(cons (car cards) (car rest))) (cdr rest))))

    ; ((6 H) (7 H)) -> (((6 H)) ((7 H)))
    (t
     (append `((,(car cards))) (groups (cdr cards)))))
  )

(defun group-lengths (groups)
  (setf groups (sort groups #'> :key #'length))
  (mapcar #'length groups))

(defun four-of-a-kind-p (hand)
  (equal '(4 1) (group-lengths (groups hand))))

(defun full-house-p (hand)
  (equal '(3 2) (group-lengths (groups hand))))

(defun three-of-a-kind-p (hand)
  (equal '(3 1 1) (group-lengths (groups hand))))

(defun two-pair-p (hand)
  (equal '(2 2 1) (group-lengths (groups hand))))

(defun pair-p (hand)
  (equal '(2 1 1 1) (group-lengths (groups hand))))

(defun hand-type (hand)
  (let* ((hand (sort hand #'> :key #'card-value))
         (straight (straight-p hand))
         (flush (flush-p hand)))
    (cond
      ((and straight flush) 'straight-flush)
      ((four-of-a-kind-p hand) 'four-of-a-kind)
      ((full-house-p hand) 'full-house)
      (flush 'flush)
      (straight 'straight)
      ((three-of-a-kind-p hand) 'three-of-a-kind)
      ((two-pair-p hand) 'two-pair)
      ((pair-p hand) 'pair)
      (t 'high-card)))
  )

(defun hand-rank (hand-type)
  (cond
    ((eq 'straight-flush hand-type) 1)
    ((eq 'four-of-a-kind hand-type) 2)
    ((eq 'full-house hand-type) 3)
    ((eq 'flush hand-type) 4)
    ((eq 'straight hand-type) 5)
    ((eq 'three-of-a-kind hand-type) 6)
    ((eq 'two-pair hand-type) 7)
    ((eq 'pair hand-type) 8)
    (t 9))
  )

(defun higher-card (a b)
  (setf a (sort a #'> :key #'card-value))
  (setf b (sort b #'> :key #'card-value))
  (cond
    ((or (null a) (null b)) nil)
    (t (let ((value-a (card-value (car a)))
             (value-b (card-value (car b))))
         (cond
           ((not (= value-a value-b)) (> value-a value-b))
           (t (higher-card (cdr a) (cdr b)))))))
  )

(defun compare-hands (a b)
  (let ((type-a (hand-type a))
        (type-b (hand-type b)))
      (cond
        ((eq type-a type-b) (higher-card a b))
        (t (< (hand-rank type-a) (hand-rank type-b)))))
  )

(defun rank-hands (hands)
  (sort hands #'compare-hands))

(defun enumerate (lst)
  (loop for elt in lst and i from 0
        collect (cons i elt)))

(defun winning-hand (hands)
  (car (rank-hands hands)))

(def-suite test-poker-hands :description "Test poker hand ranking")

(in-suite test-poker-hands)

(test card-value
  (is (equal '(2 6 9 11 13) (mapcar #'card-value '((2 H) (6 D) (9 S) (J C) (K D)))))
  )

(test sequential-p
  (is (sequential-p '(5 4 3 2 1)))
  (is (not (sequential-p '(1 3 2 5 4))))
  )

(test straight-p
  (is (straight-p '((2 H) (3 D) (4 S) (5 C) (6 D))))
  (is (not (straight-p '((A H) (K H) (Q H) (J H) (9 H)))))
  )

(test suit-value
  (is (equal '(1 2 3 4 2) (mapcar #'suit-value '((2 C) (3 D) (4 H) (5 S) (6 D)))))
  )

(test flush-p
  (is (flush-p '((A H) (K H) (Q H) (J H) (10 H))))
  (is (not (flush-p '((A H) (K D) (Q S) (J C) (10 H)))))
  )

(test hand-type
  (is (eq 'straight-flush (hand-type '((A H) (K H) (Q H) (J H) (10 H)))))
  (is (eq 'four-of-a-kind (hand-type '((A H) (K H) (A C) (A D) (A S)))))
  (is (eq 'full-house (hand-type '((A H) (K H) (A C) (A D) (K S)))))
  (is (eq 'flush (hand-type '((A H) (K H) (Q H) (J H) (9 H)))))
  (is (eq 'straight (hand-type '((A S) (K C) (Q H) (J D) (10 S)))))
  (is (eq 'three-of-a-kind (hand-type '((A H) (A C) (A D) (K C) (Q C)))))
  (is (eq 'two-pair (hand-type '((A H) (A C) (K D) (K C) (Q C)))))
  (is (eq 'pair (hand-type '((A H) (A C) (K D) (9 C) (Q C)))))
  (is (eq 'high-card (hand-type '((A S) (10 C) (9 H) (6 D) (2 D)))))
  )

(test groups
  (is (equal '(((2 H) (2 D) (2 S)) ((9 C)) ((K D)))
             (groups '((2 H) (2 D) (2 S) (9 C) (K D)))))
  (is (equal '(((2 H)) ((3 D)) ((5 S)) ((9 C)) ((K D)))
             (groups '((2 H) (3 D) (5 S) (9 C) (K D)))))
  (is (equal '(((2 H) (2 D) (2 S) (2 C)) ((K D)))
             (groups '((2 H) (2 D) (2 S) (2 C) (K D)))))
  )

(test four-of-a-kind-p
  (is (four-of-a-kind-p '((2 H) (2 D) (2 S) (2 C) (K D))))
  )

(test full-house-p
  (is (full-house-p '((2 H) (2 D) (2 C) (K D) (K C))))
  )

(test three-of-a-kind-p
  (is (three-of-a-kind-p '((2 H) (2 D) (2 C) (K D) (Q C))))
  )

(test two-pair-p
  (is (two-pair-p '((2 H) (2 D) (K C) (K D) (Q C))))
  )

(test pair-p
  (is (pair-p '((2 H) (2 D) (9 C) (K D) (Q C))))
  )

(test higher-card
  (is (higher-card '((A D) (3 C) (4 S) (7 C) (9 D))
                   '((K D) (3 H) (4 D) (7 S) (9 C))))
  (is (higher-card '((A D) (Q S) (4 S) (7 C) (9 D))
                   '((A C) (J C) (4 D) (7 S) (9 C))))
  (is (higher-card '((9 D) (7 C) (4 S) (3 C) (A D))
                   '((K D) (3 H) (4 D) (7 S) (9 C))))
  (is (higher-card '((A D) (4 C) (4 S) (7 C) (9 D))
                   '((A C) (3 H) (4 D) (7 S) (9 C))))
  )

(test winning-hand
  (is (equal '((A H) (K H) (Q H) (J H) (10 H))
             (winning-hand '(((A H) (K H) (Q H) (J H) (10 H))
                             ((K D) (3 H) (4 D) (7 S) (9 C))
                             ))))
  (is (equal '((A H) (A C) (K H) (K C) (9 D))
             (winning-hand '(((A H) (A C) (K H) (K C) (9 D))
                             ((A S) (A D) (K S) (K D) (8 D))
                             ))))
  )

(run! 'test-poker-hands)

;;; L-99: Ninety-Nine Lisp Problems
;;; http://www.ic.unicamp.br/~meidanis/courses/mc336/2006s2/funcional/L-99_Ninety-Nine_Lisp_Problems.html
;;;

(in-package :cl-user)
(defpackage :cl-99
  (:use :cl :cl-arrows))
(in-package :cl-99)

;;; Working with lists
(defun my-last (lst)
  "Find the last box of a list."
  (-> (length lst)
    1-
    (nth lst)))

(defun my-but-last (lst)
  "Find the last but one box of a list."
  (-> (length lst)
    (- 2)
    (nthcdr lst)))

(defun element-at (lst k)
  "Find the K'th element of a list.
The first element in the list is number 1."
  (if (= k 1)
      (car lst)
      (element-at (cdr lst) (1- k))))

(defun my-length (lst)
  "Find the number of elements of a list."
  (->> lst (mapcar (constantly 1)) (reduce #'+)))

(defun my-reverse (lst)
  "Reverse a list."
  (reduce (lambda (x y) (cons y x)) lst :initial-value nil))

(defun palindrome-p (lst)
  "Find out whether a list is a palindrome."
  (equal lst (reverse lst)))

(defun my-flatten (lst)
  "Flatten a nested list structure."
  (if (listp lst)
      (mapcan #'my-flatten lst)
      (list lst)))

(defun compress (lst)
  "Eliminate consecutive duplicates of list elements."
  (-> (lambda (r x)
        (if (eq (car r) x)
            r
            (cons x r)))
    (reduce lst :initial-value (list (car lst)))
    nreverse))

(defun pack (lst)
  "If a list contains repeated elements they should be placed in separate sublists."
  (destructuring-bind (fst . rest) lst
    (-> (lambda (r x)
         (destructuring-bind ((v . vs) . others) r
           (if (eq v x)
               (cons (cons x (cons v vs)) others)
               (cons (list x) (cons (cons v vs) others)))))
      (reduce rest :initial-value `((,fst)))
     nreverse)))

(defun encode (lst)
  "Run-length encoding of a list."
  (->> lst
    pack
    (mapcar (lambda (xs) `(,(length xs) ,(car xs))))))

(defun encode-modified (lst)
  "Modified run-length encoding."
  (->> lst
    encode
    (mapcar (lambda (xs)
              (destructuring-bind (n v) xs
                (if (= 1 n)
                    v
                    xs))))))

(defun decode (lst)
  "Decode a run-length encoded list."
  (->> lst
    (mapcan (lambda (xs)
              (if (atom xs)
                  (list xs)
                  (make-list (car xs) :initial-element (cadr xs)))))))

(defun encode-direct (lst)
  "Run-length encoding of a list (direct solution.)
Implement the so-called run-length encoding data compression method directly.
I.e. don't explitly create the sublists containing the duplicates,
as in problem P09, but only count them. As in problem P11,
simplify the result list by replacing the singleton lists (1 X) by X."
  (destructuring-bind (first . rest) lst
    (flet ((->run-length (r x)
             (destructuring-bind ((count elm) . others) r
               (if (eq elm x)
                   (cons (list (1+ count) elm) others)
                   (cons (list 1 x) (cons (list count elm) others)))))
           (simplify (x)
             (if (= 1 (car x)) (cadr x) x)))
      (->> (reduce #'->run-length rest :initial-value `((0 ,first)))
        (mapcar #'simplify)))))

(defun dupli (lst)
  "Duplicate the elements of a list."
  (mapcan (lambda (x) (make-list 2 :initial-element x)) lst))

(defun repli (lst n)
  "Replicate the elements of a list a given number of times."
  (mapcan (lambda (x) (make-list n :initial-element x)) lst))

(defun drop (lst n)
  "Drop every N'th element from a list."
  (loop for i from 1
        for x in lst
        when (not (zerop (rem i n)))
          collect x))

(defun split (lst n)
  "Split a list into two parts; the length of the first part is given."
  (loop for x in lst
        for i from 0
        if (< i n)
          collect x into pre
        else
          collect x into suf
        finally
           (return (list pre suf))))

(defun slice (lst i k)
  "Extract a slice from a list.
Given two indices, I and K, the slice is the list containing the elements
between the I'th and K'th element of the original list (both limits included).
Start counting the elements with 1."
  (loop for n from 1
        for x in lst
        when (>= k n i)
          collect x))

(defun rotate (lst n)
  "Rotate a list N places to the left."
  (let ((splitter (if (or (zerop n) (plusp n))
                      n
                      (+ (length lst) n))))
    (->> splitter
      (split lst)
      nreverse
      (apply #'append))))

(defun remove-at (lst k)
  "Remove the K'th element from a list."
  (loop for i from 1
        for x in lst
        when (not (= i k))
          collect x))

(defun insert-at (v xs n)
  "Insert an element at a given position into a list."
  (loop for i from 1
        for x in xs
        if (= i n)
          collect (list v x) into r
        else
          collect x into r
        finally (return (my-flatten r))))

(defun range (from to)
  "Create a list containing all integers within a given range."
  (loop for i from from upto to collect i))

(defun rnd-select (lst n)
  "Extract a given number of randomly selected elements from a list."
  (if (zerop n)
      nil
      (let ((rnd (-> lst length random 1+)))
        (cons (element-at lst rnd)
              (rnd-select (remove-at lst rnd) (1- n))))))

(defun lotto-select (n m)
  "Lotto: Draw N different random numbers from the set 1..M."
  (-> (range 1 m)
    (rnd-select n)))

(defun rnd-permu (lst)
  "Generate a random permutation of the elements of a list."
  (rnd-select lst (length lst)))

(defun combination (k lst)
  "Generate the combinations of K distinct objects chosen from the N elements of a list.
In how many ways can a committee of 3 be chosen from a group of 12 people?
We all know that there are C(12,3) = 220 possibilities (C(N,K) denotes
the well-known binomial coefficients). For pure mathematicians, this result may be great.
But we want to really generate all the possibilities in a list."
  ;; from https://www.geeksforgeeks.org/print-all-possible-combinations-of-r-elements-in-a-given-array-of-size-n/
  (let ((current (make-list k))
        (result nil))
    (labels ((rec (start end index)
               (if (= index k)
                   (push (copy-list current) result)
                   (loop for i from start
                         while (and (<= i end)
                                    (>= (- end (1+ i))
                                        (- k index)))
                         do (setf (nth index current) (nth i lst))
                            (rec (1+ i) end (1+ index))))))
      (rec 0 (1+ (length lst)) 0)
      (nreverse result))))

(defun lsort (lst)
  "Sorting a list of lists according to length of sublists"
  (sort lst #'< :key #'length))

(defun lfsort (lst)
  "Sorting a list of lists according to their length frequency of sublists."
  (let ((freq (make-hash-table)))
    (loop for x in lst
          do (let ((key (length x)))
               (multiple-value-bind (val set?) (gethash key freq)
                 (if set?
                     (setf (gethash key freq) (1+ val))
                     (setf (gethash key freq) 1)))))
    (flet ((lookup (x) (gethash (length x) freq)))
      (sort lst #'< :key #'lookup))))

(defun is-prime (n)
  "Determine whether a given integer number is prime."
  (let ((divisibles
          (loop for i from n downto 1
                when (zerop (mod n i))
                  collect i)))
    (= 2 (length divisibles))))

(defun my-gcd (x y)
  "Determine the greatest common divisor of two positive integer numbers."
  ;; Euclidean Algorithm
  (if (< x y)
      (my-gcd y x)
      (let ((reminder (rem x y)))
        (if (zerop reminder)
            y
            (my-gcd y reminder)))))

(defun coprime (x y)
  "Determine whether two positive integer numbers are coprime."
  (= 1 (my-gcd x y)))

(defun totient-phi (n)
  "Calculate Euler's totient function phi(m)."
  (->> (range 1 n)
    (remove-if-not (lambda (x) (coprime x n)))
    length))

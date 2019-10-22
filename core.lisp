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

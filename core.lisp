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

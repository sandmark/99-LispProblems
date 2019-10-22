(in-package :cl-user)
(defpackage :cl-99-asd
  (:use :cl :asdf))
(in-package :cl-99-asd)

(defsystem :cl-99
  :class :package-inferred-system
  :description "Ninety-Nine Lisp Problems"
  :version "0.1"
  :author "sandmark"
  :license "Public Domain"
  :depends-on (:cl-arrows "main.lisp"))

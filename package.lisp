;;;; package.lisp

(cl:in-package :cl-user)

(progn
  (defpackage :srfi-95
    (:use)
    (:export . #1=(:sorted?
                   :sort
                   :sort!
                   :merge
                   :merge! )))

  (defpackage :srfi-95.internal
    (:use :srfi-95 :cl :fiveam)
    (:shadowing-import-from :srfi-5 :let)
    (:shadowing-import-from :srfi-95
                            . #1#)
    (:shadow :loop :step) ))




;;;; package.lisp

(cl:in-package :cl-user)

(defpackage "https://github.com/g000001/srfi-95"
  (:use)
  (:export
   sorted?
   sort
   sort!
   merge
   merge!))

(defpackage "https://github.com/g000001/srfi-95#internals"
  (:use "https://github.com/g000001/srfi-95"
        cl
        fiveam)
  (:shadowing-import-from "https://github.com/g000001/srfi-5"
                          let)
  (:shadowing-import-from "https://github.com/g000001/srfi-95"
                          sort
                          merge)
  (:shadow loop step))




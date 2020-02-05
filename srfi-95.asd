;;;; srfi-95.asd

(cl:in-package :asdf)


(defsystem :srfi-95
  :version "20200206"
  :description "SRFI 95: Sorting and Merging"
  :long-description "SRFI 95: Sorting and Merging
https://srfi.schemers.org/srfi-95"
  :author "Aubrey Jaffer"
  :maintainer "CHIBA Masaomi"
  :license "Unlicense"
  :serial t
  :depends-on (:fiveam :srfi-5)
  :components ((:file "package")
               (:file "srfi-95")
               (:file "test")))


(defmethod perform :after ((o load-op) (c (eql (find-system :srfi-95))))
  (let ((name "https://github.com/g000001/srfi-95")
        (nickname :srfi-95))
    (if (and (find-package nickname)
             (not (eq (find-package nickname)
                      (find-package name))))
        (warn "~A: A package with name ~A already exists." name nickname)
        (rename-package name name `(,nickname)))))


(defmethod perform ((o test-op) (c (eql (find-system :srfi-95))))
  (let ((*package*
         (find-package
          "https://github.com/g000001/srfi-95#internals")))
    (eval
     (read-from-string
      "
      (or (let ((result (run 'srfi-95)))
            (explain! result)
            (results-status result))
          (error \"test-op failed\") )"))))


;;; *EOF*

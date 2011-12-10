;;;; srfi-95.asd

(cl:in-package :asdf)

(defsystem :srfi-95
  :serial t
  :depends-on (:fiveam)
  :components ((:file "package")
               (:file "srfi-95")
               (:file "test")))

(defmethod perform ((o test-op) (c (eql (find-system :srfi-95))))
  (load-system :srfi-95)
  (or (flet ((_ (pkg sym)
               (intern (symbol-name sym) (find-package pkg))))
         (let ((result (funcall (_ :fiveam :run) (_ :srfi-95.internal :srfi-95))))
           (funcall (_ :fiveam :explain!) result)
           (funcall (_ :fiveam :results-status) result)))
      (error "test-op failed") ))


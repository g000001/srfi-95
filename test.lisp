(cl:in-package :srfi-95.internal)

(def-suite srfi-95)

(in-suite srfi-95)


(test sort
  (for-all ((list (gen-list :length (gen-integer :min 0
                                                 :max 1000)
                            :elements (gen-integer :min most-negative-fixnum
                                                   :max most-positive-fixnum))))
    (is (equal (cl:sort (copy-list list) #'<)
               (sort (copy-list list) #'<)))
    (is (equal (cl:sort (copy-list list) #'>=)
               (sort list #'>=)))
    (is (equal (cl:sort (copy-list list) #'>=)
               (sort! (copy-list list) #'>=)))))

(test stable?
  (for-all ((list (gen-list :length (gen-integer :min 0
                                                 :max 1000)
                            :elements (gen-integer :min most-negative-fixnum
                                                   :max most-positive-fixnum))))
    (let ((list1 (cl:stable-sort (copy-list list) #'< :key #'integer-length))
          (list2 (sort! (copy-list list) #'< #'integer-length))
          (list3 (sort list #'< #'integer-length)))
      (is (equal list1 list2))
      (is (equal list1 list3)))
    (let ((list1 (cl:stable-sort (copy-list list) #'>= :key #'integer-length))
          (list2 (sort! (copy-list list) #'>= #'integer-length))
          (list3 (sort list #'>= #'integer-length)))
      (is (equal list1 list2))
      (is (equal list1 list3)))) )

(test sorted?
  (for-all ((list (gen-list :length (gen-integer :min 0
                                                 :max 1000)
                            :elements (gen-integer :min most-negative-fixnum
                                                   :max most-positive-fixnum))))
    (is-true (sorted? (cl:sort list #'<) #'<))))


(test merge
  (for-all ((list1 (gen-list :length (gen-integer :min 0
                                                 :max 1000)
                            :elements (gen-integer :min most-negative-fixnum
                                                   :max most-positive-fixnum)))
            (list2 (gen-list :length (gen-integer :min 0
                                                  :max 1000)
                             :elements (gen-integer :min most-negative-fixnum
                                                    :max most-positive-fixnum))))
    (is (equal (cl:merge 'list
                         (cl:sort (copy-list list1) #'<)
                         (cl:sort (copy-list list2) #'<)
                         #'<)
               (merge (sort list1 #'<)
                      (sort list2 #'<)
                      #'<)))
    ;; stable
    (is (equal (cl:merge 'list
                         (cl:stable-sort (copy-list list1)
                                         #'<
                                         :key #'integer-length)
                         (cl:stable-sort (copy-list list2)
                                         #'<
                                         :key #'integer-length)
                         #'<)
               (merge (sort list1 #'< #'integer-length)
                      (sort list2 #'< #'integer-length)
                      #'<))) ))

;;; eof

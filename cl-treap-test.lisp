(defpackage :cl-treap-test
  (:use :cl :fiveam :cl-treap))
(in-package :cl-treap-test)

(def-suite cl-treap)
(in-suite cl-treap)

(test treap-methods
  (is (equal 5 (value (root (insert (make-treap) 5)))))
  (is (equal 5 (value (root (insert (insert (make-treap) 5 :priority 0)
                                    0 :priority 1)))))
  (is (equal 5 (value (right (root (insert (insert (make-treap) 5 :priority 1)
                                           0 :priority 0))))))
  (is (equal 0 (value (left (root (insert (insert (make-treap) 5 :priority 0)
                                           0 :priority 1)))))))
(test treap-find
  (is (equal nil (treap-find nil 1)))
  (is (equal t (treap-find (insert (make-treap) 1) 1)))
  (is (equal t (treap-find (insert (insert (insert (make-treap) 1) 2) 3) 2))))

(test treap-test-functions
  (is (treap-find (insert (make-treap :order #'string> :test #'string=) "blub") "blub")))

(test all-inserts-can-be-found
  (finishes
    (let ((treap (make-treap :test #'string= :order #'string>)))
      (loop for i from 0 to 100000
         do (insert treap (princ-to-string i)))
      (loop for i from 0 to 100000
         when (not (treap-find treap (princ-to-string i)))
         do (error "should have found item in treap")))))

(run!)

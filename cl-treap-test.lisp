(defpackage :cl-treap-test
  (:use :cl :fiveam :cl-treap))
(in-package :cl-treap-test)

(def-suite cl-treap)
(in-suite cl-treap)

(test treap-methods
  (is (equal 5 (key (root (insert (make-treap) 5 t)))))
  (is (equal 5 (key (root (insert (insert (make-treap) 5 t :priority 0)
                                    0 t :priority 1)))))
  (is (equal 5 (key (right (root (insert (insert (make-treap) 5 t :priority 1)
                                           0 t :priority 0))))))
  (is (equal 0 (key (left (root (insert (insert (make-treap) 5 t :priority 0)
                                           0 t :priority 1)))))))
(test treap-find
  (is (equal nil (treap-find nil 1)))
  (is (equal t (treap-find (insert (make-treap) 1 t) 1)))
  (is (equal t (treap-find (insert (insert (insert (make-treap) 1 t) 2 t) 3 t) 2))))

(test treap-find-key-value
  (is (equal "blub" (treap-find (insert (make-treap) 1 "blub") 1))))

(test treap-test-functions
  (is (treap-find (insert (make-treap :order #'string> :test #'string=) "blub" t) "blub")))

(test all-inserts-can-be-found
  (finishes
    (let ((treap (make-treap :test #'string= :order #'string>)))
      (loop for i from 0 to 10000
         do (insert treap (princ-to-string i) t))
      (loop for i from 0 to 10000
         when (not (treap-find treap (princ-to-string i)))
         do (error "should have found item in treap")))))

(run!)

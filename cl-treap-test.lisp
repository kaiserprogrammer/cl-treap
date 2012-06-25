(defpackage :cl-treap-test
  (:use :cl :fiveam :cl-treap))
(in-package :cl-treap-test)

(def-suite cl-treap)
(in-suite cl-treap)

(test treap-insert
  (is (equal 100 (priority (make-treap :priority 100))))
  (is (not (equal nil (priority (make-treap)))))
  (is (equal nil (right (make-treap))))
  (is (equal 10 (priority (right (make-treap :right (make-treap :priority 10))))))
  (is (equal 11 (priority (left (make-treap :left (make-treap :priority 11))))))
  (is (equal 12 (value (make-treap :value 12)))))

(test treap-methods
  (is (equal 'value (value (insert nil 'value))))
  (is (equal 5 (value (insert (make-treap :value 5 :priority 0) 0
                              :priority 1))))
  (is (equal 5 (value (right (insert (make-treap :value 5 :priority 1) 0
                                     :priority 0))))))

(test treap-find
  (is (equal nil (treap-find nil 1)))
  (is (equal t (treap-find (insert nil 1) 1)))
  (is (equal t (treap-find (insert (insert (insert nil 1) 2) 3) 2))))

(run!)

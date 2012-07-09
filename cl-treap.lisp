(defpackage :cl-treap
  (:use :cl)
  (:export
   :make-node
   :priority
   :right
   :left
   :value
   :insert
   :treap-find
   :insertf
   :make-treap
   :root
   :key))
(in-package :cl-treap)


(defun make-treap (&key (test #'=) (order #'>))
  (cons test (cons order nil)))

(defun test (treap)
  (first treap))

(defun order (treap)
  (second treap))

(defun root (treap)
  (cddr treap))

(defun make-node (&key (priority (random 9223372036854775807))
                    right
                    left
                    key
                    value)
  (cons priority (cons left (cons right (cons key value)))))

(defun priority (node)
  (first node))

(defun right (node)
  (third node))

(defun left (node)
  (second node))

(defun key (node)
  (fourth node))

(defun value (node)
  (cddddr node))

(defun left-rotate (treap)
  (make-node :priority (priority (left treap))
             :key (key (left treap))
             :value (value (left treap))
             :left (left (left treap))
             :right (make-node
                     :priority (priority treap)
                     :key (key treap)
                     :value (value treap)
                     :left (right (left treap))
                     :right (right treap))))

(defun right-rotate (treap)
  (make-node :priority (priority (right treap))
             :key (key (right treap))
             :value (value (right treap))
              :left (make-node
                     :priority (priority treap)
                     :key (key treap)
                     :value (value treap)
                     :left (left treap)
                     :right (left (right treap)))
              :right (right (right treap))))

(defun insert (treap key value &key (priority (random 9223372036854775807)))
  (setf (cddr treap)
        (insert-node (root treap)
                     key
                     value
                     :priority priority
                     :order (order treap)
                     :test (test treap)))
  treap)

(defun insert-node (treap key value &key (priority (random 9223372036854775807))
                                                    order
                                                    test)
  (cond ((null treap)
         (make-node :priority priority
                    :key key
                    :value value))
        ((funcall order key (key treap))
         (let ((new (make-node :priority (priority treap)
                               :key (key treap)
                               :value (value treap)
                               :left (left treap)
                               :right (insert-node
                                       (right treap)
                                       key
                                       value
                                       :priority priority
                                       :order order
                                       :test test))))
           (if (< (priority (right new))
                  (priority new))
               (right-rotate new)
               new)))
        ((not (funcall order key (key treap)))
         (let ((new (make-node :priority (priority treap)
                               :key (key treap)
                               :value (value treap)
                               :left (insert-node
                                      (left treap)
                                      key
                                      value
                                      :priority priority
                                      :order order
                                      :test test)
                               :right (right treap))))
           (if (< (priority (left new))
                  (priority new))
               (left-rotate new)
               new)))

        ((funcall test key (key treap))
         treap)))

(defun treap-find (treap key)
  (if (null treap)
      nil
      (node-find (root treap) key :order (order treap) :test (test treap))))

(defun node-find (node key &key order test)
  (cond ((null node) nil)
        ((funcall test (key node) key) (value node))
        ((funcall order (key node) key) (node-find (left node) key :order order :test test))
        (t (node-find (right node) key :order order :test test))))

(defun deepness (treap)
  (if (null treap)
      0
      (deepness-node (root treap))))

(defun deepness-node (node)
  (if (null node)
      0
      (+ 1 (max (deepness-node (left node))
                (deepness-node (right node))))))

(defun size (treap)
  (if (null treap)
      0
      (size-node (root treap))))

(defun size-node (node)
  (if (null node)
      0
      (+ 1
         (size-node (left node))
         (size-node (right node)))))

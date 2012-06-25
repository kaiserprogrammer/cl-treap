(defpackage :cl-treap
  (:use :cl)
  (:export
   :make-treap
   :priority
   :right
   :left
   :value
   :insert
   :treap-find))
(in-package :cl-treap)

(defun make-treap (&key (priority (random 9223372036854775807))
                     right
                     left
                     value)
  (list priority left right value))

(defun priority (treap)
  (first treap))

(defun left-rotate (treap)
  (make-treap :priority (priority (left treap))
              :value (value (left treap))
              :left (left (left treap))
              :right (make-treap
                      :priority (priority treap)
                      :value (value treap)
                      :left (right (left treap))
                      :right (right treap))))

(defun right-rotate (treap)
  (make-treap :priority (priority (right treap))
              :value (value (right treap))
              :left (make-treap
                     :priority (priority treap)
                     :value (value treap)
                     :left (left treap)
                     :right (left (right treap)))
              :right (right (right treap))))

(defun right (treap)
  (third treap))

(defun left (treap)
  (second treap))

(defun value (treap)
  (fourth treap))

(defun insert (treap value &key (priority
                                  (random 9223372036854775807)))
  (cond ((null treap)
         (make-treap :priority priority
                     :value value))
        ((< value (value treap))
         (let ((new (make-treap :priority (priority treap)
                                :value (value treap)
                                :left (insert
                                       (left treap)
                                       value
                                       :priority priority)
                                :right (right treap))))
           (if (< (priority (left new))
                  (priority new))
               (left-rotate new)
               new)))
        ((> value (value treap))
         (let ((new (make-treap :priority (priority treap)
                                :value (value treap)
                                :left (left treap)
                                :right (insert
                                        (right treap)
                                        value
                                        :priority priority))))
           (if (< (priority (right new))
                  (priority new))
               (right-rotate new)
               new)))
        ((= value (value treap))
         treap)))


(defun insertf (treap value &key (priority
                                 (random 9223372036854775807)))
  (cond ((null treap)
         (make-treap :value value :priority priority))
        ((< (priority treap) priority)
         (progn (if (> (value treap) value)
               (setf (second treap) (insertf (left treap) value :priority priority))
               (setf (third treap)
                     (insertf (right treap) value :priority priority)))
                treap))
        ((> (priority treap) priority)
         (if (> (value treap) value)
             (make-treap :value value :priority priority :right treap)
             (make-treap :value value :priority priority :left treap)))))

(defun treap-find (treap key)
  (cond ((null treap) nil)
        ((= (value treap) key) t)
        ((> (value treap) key) (treap-find (left treap) key))
        (t (treap-find (right treap) key))))

(defun deepness (treap)
  (if (null treap)
      0
      (+ 1 (max (deepness (left treap))
                (deepness (right treap))))))

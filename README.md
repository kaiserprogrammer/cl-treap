# cl-treap

is an implementation of a treap which combines the properties of a binary tree and a heap.

## Usage

```lisp
(defvar *treap* nil)
(setf *treap* (insert *treap* 1))
(setf *treap* (insert *treap* 50))
(setf *treap* (insert *treap* 60))
(treap-find *treap* 1) => t
(treap-find *treap* 55) => nil
```

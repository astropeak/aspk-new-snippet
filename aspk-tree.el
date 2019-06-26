;; Implementation of a N- tree.
;; Data structure of the tree:
;; (root (child_1 (cchild_1)) (child_2) )
;; If `tree' is a tree, then (car tree) is the root element, (cdr tree) is the list of it's children, which are all trees.

;; TODO: make some tree functions from destructive to non-destructive. Then the head node will not needed.
(defconst aspk/tree-head-element ">ROOT_OF_TREE<")

(defun aspk/tree-head-element-p (elt)
  "Judge whether `elt' is a tree head element"
  (and (stringp elt)
       (string= elt aspk/tree-head-element)))

(defun aspk/tree-get-element-and-parent (tree elt &optional predicate parent)
  "Find in a tree an element and its parent subtree. The car is the element subtree, the cdr is its parent subtree. nil means not exist, both for the element or its partent. the `parent' parameter is not given, then `tree' is the root. The parameter will not given in most cases. `predicate' is the compare function who takes two args,elm1 and elm2, if nil, eq will be used"
  (or predicate (setq predicate 'eq))

  (and tree
       (if (funcall predicate (car tree) elt)
           (cons tree parent)

         ;; TODO: write a macro: dolist-if. Its form is (dolist-if (var list condition) BODY... ). Same as dolist except check condition is true before every iteration.
         (let ((child-list (cdr tree))
               (rst))
           (while (and child-list (not rst))
             (setq rst (aspk/tree-get-element-and-parent (car child-list) elt predicate tree))
             (setq child-list (cdr child-list)))
           rst))))

(defun aspk/tree-get-element (tree elt)
  "Get this element(a pointer to it, modify this element will modify the tree) of the given `elt' in `tree'"
  (car
   (car (aspk/tree-get-element-and-parent tree elt 'equal)))) ;this element tree

(defun aspk/tree-get-parent (tree elt)
  "Get the parent element of the given `elt' in `tree'"
  (car
   (cdr (aspk/tree-get-element-and-parent tree elt 'equal)))) ;the parent tree

(defun aspk/tree-get-children (tree elt)
  "Get the child elements of the given `elt' in `tree'"
  (mapcar 'car
          (cdr						   ;the list of children of this element
           (car (aspk/tree-get-element-and-parent tree elt 'equal)))))


(defun aspk/tree-add-element (tree parent-elt elt)
  "Add `elt' as a child of `parent-elt' element, in `tree'."
  (let ((parent-subtree
         (car (aspk/tree-get-element-and-parent tree parent-elt 'equal))))
    (and parent-subtree
         (listp parent-subtree)
         (nconc parent-subtree (list (list elt))))))

;; TODO: This function may not needed anymore.
(defun aspk/tree-add-child (parent elt)
  "Add `elt' as a child of `parent', which is a tree. Return the new created child tree"
  (and (listp parent)
       (let ((rst (list elt)))
         (nconc parent (list rst))
         rst)))

(defun aspk/tree-delete-element (tree elt)
  "Delete `elt' from tree. Children of `elt' will become child of `elt''s parent. Return the parent element of `elt'(maybe the head element), if `elt' not exist in `tree', return nil, if `elt' is head element, return head element"
  (if (aspk/tree-head-element-p elt)
      ;; Head element can't be deleted.
      (progn
        (message "Error: head element can't be deleted")
        elt)

    (let* ((rst (aspk/tree-get-element-and-parent tree elt 'equal))
           (subtree (car rst))
           (parent-tree (cdr rst)))

      (message "subtree=%s, parent-tree=%s"
               subtree parent-tree)

      ;; parent of `elt' will always not be nil, because we have a head node
      ;; Add all children of `elt' to its parent
      (dolist (e (cdr subtree))
        (nconc parent-tree (list e))
        (message "e=%s, parent-tree=%s" e parent-tree))

      (delete subtree parent-tree)
      (message "subtree=%s, parent-tree=%s"
               subtree parent-tree)

      (car parent-tree))))

(defun aspk/tree-decendent-p (tree elt1 elt2)
  "Check if elt1 is a decendent of elt2 in the tree"
  )

(defun aspk/tree-move-subtree (tree elt parent-elt)
  "Move the subtree whose root is `elt' to the tree whose root is `parent-elt'. BUG: if we move a element subtree to a child of it, there will be wrong. There will be a ring. So we must ensure parent is not a decendent of element before moving."
  (let* ((rst (aspk/tree-get-element-and-parent tree elt 'equal))
         (subtree (car rst))
         (parent-subtree-old (cdr rst))
         (parent-subtree-new (car (aspk/tree-get-element-and-parent tree parent-elt 'equal))))
    (when (and subtree parent-subtree-new)
      (if (eq parent-subtree-new parent-subtree-old)
          (message "'%s' already the child of '%s', no needed to move" elt parent-elt)
        (nconc parent-subtree-new (list subtree))
        (delete subtree parent-subtree-old)))))

(defun aspk/tree-create ()
  (cons aspk/tree-head-element nil))

(defun aspk/tree-print (tree &optional fn fn-header depth)
  "Print the tree in current buffer. `fn' is a function to convert the element to its printed string. `depth' is the orignal depth of print. `fn-header' is a function construct the header of each printed line"
  (or fn (setq fn (lambda (e) e)))
  (or depth (setq depth 0))
  (or fn-header (setq fn-header (lambda (d) (make-string d ?*))))

  (and tree
       (progn
         (message (format "%s%s"
                         (funcall fn-header depth)
                         (funcall fn (car tree))))

         (let ((child-list (cdr tree)))
           (while child-list
             (aspk/tree-print (car child-list) fn fn-header (+ depth 1))
             (setq child-list (cdr child-list)))))))

(defun aspk/tree-iterate (tree fn &optional -------parent-subtree depth)
  "Iterate all element in the TREE, with function FN. The element's containing substree, its parent subtree, and the element's depteh will be passed to the FN. PARENT is the parent sub tree of (car tree)"
  (or depth (setq depth 0))
  (when tree
    (funcall fn tree -------parent-subtree depth)
    ;; -------elem can be seen by fn, so be becauful. This is a problem of dynamic scoping.
    (mapc (lambda (-------subtree)
            (aspk/tree-iterate -------subtree fn tree (+ depth 1)))
          (cdr tree))))

;; TODO: if the parameter is fn, then there will be an error. Because it is the same in aspk/tree-iterate. This is a bad behavior of dynamic scoping
(defun aspk/tree-iterate-element-value (tree -fn)
  "Iterate all element value in the TREE, with function FN."
  (aspk/tree-iterate tree (lambda (elem-subtree parent-subtree depth)
                            (funcall -fn (car elem-subtree)))))

(provide 'aspk-tree)

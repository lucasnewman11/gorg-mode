
;This file contains functions which extend the functionality of alists.

(provide 'alists)

 (defun overwrite-assoc-to-alist-nondestruc (association alist)
  "Accepts as arguments A) a cons cell containing an association, and B) an alist.  Will overwrite A to a copy of B and return the result."
  (let ((alist-with-delete nil)
	(alist-copy (copy-tree alist))) ;makes a copy of existing-alist, so that the function isn't destructive
    (set 'alist-with-delete 
	 (delete (assoc (car association) alist-copy) alist-copy))
    (append alist-with-delete `(,association))))

; This file contains core utilities for reading gorg data from files, writing gorg data to files, and manipulating gorg data.  

; gorg data is implemented in Elisp as nested association lists.  As a result, all usable subsets of gorg data are either a cons cell or an alist.  The following are the recognized units of gorg data:
;    gorg-entry-list : an alist containing individual gorg entries
;    gorg-entry : a cons cell with an indexstring as the car and a gorg attributes list as the cdr
;    gorg-attributes-list : an alist containing individual gorg attributes
;    gorg-attribute : a cons cell with an attribute name as the car and the attribute body as the cdr
;    gorg-data : any of the above

;CODE:

(provide 'gorg-utilities)

;;;;;;;;READING AND WRITING FROM GORG FILES;;;;;;;;
; Functions that deal with translating between JSON files and live Elisp data

;Reading
(defun gorg-read-data-from-filepath (filepath-string)
  "Accepts as argument a string containing a filepath to read from.  Parses the specified file as a json string and returns a gorg entry list."
  (require 'json)
  (let ((gorg-entries nil)
	 (json-key-type 'string)		
         ;; (json-object-type 'plist)
			  )
    (set-buffer (generate-new-buffer "gorg-json-text")) 
    (insert-file-contents filepath-string)
    (set 'gorg-entries (json-read-from-string (buffer-string)))
    (kill-buffer "gorg-json-text")
    gorg-entries))

;Writing
(defun gorg-write-data-to-filepath (gorg-data filepath-string)
  "Accepts as arguments A) gorg data, and B) a string containing a filepath to write to.  Writes the gorg data to the file as a json string."
  (require 'json)
  (set-buffer (generate-new-buffer "gorg-data-text"))
  (insert (json-encode gorg-data))
  (write-region nil nil filepath)
  (kill-buffer "gorg-data-text"))

;;;;;;;;;READING AND WRITING GORG ENTRIES TO GORG ENTRY LISTS;;;;;;;;
; Functions that deal with accessing and modifying individual entries in gorg entry lists.  All functions are non-destructive - modified data will be returned as a copy.

;;;Reading;;;

;All gorg entries use an indexstring as their car.  Indexstrings consist of "I<int>", where "<int>" is an integer 0 or greater.

(defun gorg-get-entry-from-entry-list (indexstring gorg-entry-list)
  "Accepts as arguments A) an indexstring, and B) a gorg entry list.  Returns a cons cell containing the gorg entry denoted by the indexstring."
  (copy-tree (assoc indexstring gorg-entry-list)))

;;;Writing;;;

(defun gorg-write-entry-to-entry-list (gorg-entry gorg-entry-list)
  "Accepts as arguments A) a gorg entry, and B) a gorg entry list.  Will write A to a copy of B using overwrite-assoc-to-alist-nondestruc, and return the result."
  (require 'alists)
  (overwrite-assoc-to-alist-nondestruc gorg-entry gorg-entry-list))

;;;;;;;;READING AND WRITING GORG ATTRIBUTES TO GORG ENTRIES;;;;;;;;
;Functions that deal with accessing and modifying individual attributes in gorg entries.  All functions are non-destructive - modified data will be returned as a copy.

;Reading
(defun gorg-get-attribute-from-entry (attribute-name-string gorg-entry)
  "Accepts as arguments A) a string containing the name of an attribute, and B) a gorg entry.  Returns a copy of the requested gorg attribute."
  (copy-tree (assoc attribute-name-string (cdr gorg-entry))))

;Writing
(defun gorg-write-attribute-to-entry (gorg-attribute gorg-entry)
  "Accepts as arguments A) a gorg attribute, and B) a gorg entry.  Returns a copy of B with A overwritten using overwrite-assoc-to-alist-nondestruc."
  (require 'alists)
  (let ((new-gorg-entry '(nil . nil)))
    (setcar new-gorg-entry (car gorg-entry))
    (setcdr new-gorg-entry (overwrite-assoc-to-alist-nondestruc gorg-attribute (cdr gorg-entry)))
    new-gorg-entry))


;;;;;;;;CREATING NEW ENTRIES FOR ENTRY LISTS;;;;;;;;
;Functions that deal with generating new entries for inclusion in existing entry lists.  The key point is that a new entry must have a unique indexstring, created by incrementing the largest index number currently in use.  Also, all gorg entries should have at least "NAME" and "BODY" attributes. 

;generating new indexstrings
(defun gorg-create-list-of-indices-from-entry-list (gorg-entry-list)
  "Accepts as argument a gorg entry list.  Returns a list of indices corresponding to the keys of each entry."
  (let ((current-index-in-alist 0)
	(current-harvested-index 0)
	(list-of-indices ()))
    (while (< current-index-in-alist (length gorg-entry-list))
      (set 'current-harvested-index 
       (string-to-number 
       (nth 1 (split-string 
	       (car (nth current-index-in-alist gorg-entry-list)) "X"))))
      (set 'list-of-indices (append list-of-indices `(,current-harvested-index)))
      (set 'current-index-in-alist (+ current-index-in-alist 1)))
    list-of-indices))

(defun create-new-indexnumber-from-list-of-indices (indices-list)
  "Accepts as argument a list of integers.  Returns an integer larger by one than the largest in the list."
  (+ (apply 'max indices-list) 1))

(defun create-indexstring-from-indexnumber (indexnumber) 
  "Accepts as argument an integer.  Returns a corresponding indexstring."
  (concat "I" (number-to-string indexnumber)))

;using an indexstring to create an entry
(defun gorg-create-entry (indexstring &optional gorg-attributes-list)
  "Accepts as argument an indexstring, and optionally, a gorg attributes list.  Returns a gorg entry with gorg attributes corresponding to the optional arg.  If no attributes list is given, then two attributes will be assigned: NAME, '', and MAIN, ''."
  (if gorg-attributes-list
      `(,indexstring . ,gorg-attributes-list)
    `(,indexstring . (("NAME" . "")("MAIN" . "")))))

(defun gorg-create-new-entry-from-entry-list (gorg-entry-list &optional gorg-attributes-list)
  "Accepts as argument A) a gorg entry list, and optionally, a gorg attributes list.  Returns a new gorg entry with an index number appropriate for inclusion in A.  Uses gorg-create-entry."
  (gorg-create-entry
   (create-indexstring-from-index
    (create-new-indexnumber-from-list-of-indices
     (gorg-create-list-of-indices-from-entry-list gorg-entry-list)))
   gorg-attributes-list))








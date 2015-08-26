;import json
(require 'json)

;;;;;;;;READING FROM GORG FILES;;;;;;;;

(defun return-greatest-string-in-list (list-of-strings)
  "Accepts as argument a list of strings.  Uses the function 'string<' to return the largest string in the list."
  (let ((current-largest nil)
	(current-candidate nil)
	(index 1))
    (set 'current-largest (nth 0 list-of-strings))
    (while (< index (length list-of-strings))
      (set 'current-candidate (nth index list-of-strings))
      (if (string< current-largest current-candidate)
	  (set 'current-largest current-candidate))
      (set 'index (+ index 1)))
    current-largest))

(defun create-gorg-filepath-to-read (directory-string)
  "Accepts as argument a string representation of a directory.  Returns the absolute filepath of the most recently created gorg file within that directory."
  (let ((files-in-directory nil))
    (set 'files-in-directory (directory-files directory-string))
    (concat directory-string "/" (return-greatest-string-in-list files-in-directory))
    ))

(defun read-gorg-file-to-data (filepath)
  "Accepts as argument a string containing a filepath to read from.  Parses the specified file as a json string and returns the resulting data structure."
  (let ((gorg-data nil)
	 (json-key-type 'string)		
         ;; (json-object-type 'plist)
			  )
    (set-buffer (generate-new-buffer "gorg-data-text")) 
    (insert-file-contents filepath)
    (set 'gorg-data (json-read-from-string (buffer-string)))
    (kill-buffer "gorg-data-text")
    gorg-data))

(defun read-gorg-from-directory (directory-string)
  "Accepts as argument a string containing a directory.  Returns the most current gorg data from that directory."
  (read-gorg-file-to-data (create-gorg-filepath-to-read directory-string)))

;;;;;;;;WRITING TO GORG FILES;;;;;;;;

(defun create-gorg-filepath-to-write (directory-string)
  "Accepts as argument a string representation of a directory.  Returns the absolute filepath of a new gorg file within that directory."
  (let ((filename . nil))
    (set 'filename (concat "gorg-" (format-time-string "%y%m%d%H%M%S") ".txt"))
    (set 'filepath (concat directory-string "/" filename))))

(defun write-gorg-data-to-file (gorg-data filepath)
  "Accepts as arguments 1) gorg data, and 2) a filepath to write to.  Writes the gorg data to the file as a json string."
  (set-buffer (generate-new-buffer "gorg-data-text"))
  (insert (json-encode gorg-data))
  (write-region nil nil filepath)
  (kill-buffer "gorg-data-text"))

(defun write-gorg-to-directory (gorg-data directory-string)
  "Accepts as arguments 1) gorg data, and 2) a string containing a directory.  Writes a new gorg file to the directory."
  (write-gorg-data-to-file gorg-data (create-gorg-filepath-to-write directory-string)))

;;;;;;;;;READING GORG DATA;;;;;;;;

(defun create-indexsymbol-from-index (index) 
  "Accepts as argument an index integer.  Returns a corresponding indexstring."
  (concat "INDEX" (number-to-string index)))

(defun get-gorg-entry (indexstring gorg-data)
  "Accepts as arguments 1) an indexstring, and 2) gorg data.  Returns a cons cell containing the denoted entry."
  (assoc indexstring gorg-data))

(defun get-gorg-entry-attribute (attribute-name gorg-entry)
  "Accepts as arguments 1) a string containing the name of an attribute, and 2) a cons cell containing a gorg-entry.  Returns a cons cell containing the denoted attribute."
  (assoc attribute-name (cdr gorg-entry)))

;;;;;;;;WRITING GORG DATA;;;;;;;

;;;Creating new entries;;;

(defun create-list-of-all-indices (gorg-data)
  "Accepts as argument gorg-data.  Returns a list of indices corresponding to the keys of each entry."
  (let ((current-index-in-alist 0)
	(current-harvested-index 0)
	(list-of-indices ()))
    (while (< current-index-in-alist (length gorg-data))
      (set 'current-harvested-index 
       (string-to-number 
       (nth 1 (split-string 
	       (car (nth current-index-in-alist gorg-data)) "X"))))
      (set 'list-of-indices (append list-of-indices `(,current-harvested-index)))
      (set 'current-index-in-alist (+ current-index-in-alist 1)))
    list-of-indices))

(defun create-new-entry-indexnumber (gorg-data)
  "Accepts as argument gorg-data.  Returns the appropriate integer for use as the index number of a new entry."
  (+ (apply 'max (create-list-of-all-indices gorg-data)) 1))

(defun create-new-entry-indexstring (gorg-data)
  "Accepts as argument gorg-data.  Returns an appropriate indexstring for use as the key of a cons cell containing a new entry."
 (concat "INDEX" (number-to-string (create-new-entry-indexnumber gorg-data))))

(defun create-new-entry-data (indexstring &optional attributes-list)
  "Accepts as argument an indexstring, and optionally, an alist of attributes.  Returns a cons cell with the indexstring as the CAR and the attributes list as the CDR.  If no attributes list is given, then two attributes will be assigned: NAME, '', and BODY, ''."
  (if attributes-list
      `(,indexstring . ,attributes-list)
    `(,indexstring . (("NAME" . "")("BODY" . "")))))
  
(defun add-new-entry-to-data (gorg-entry gorg-data)
  "Accepts as arguments 1) a cons cell containing a gorg entry, and 2) an alist containing gorg data.  Returns an alist containing gorg data, now including the new entry."
  (let ((new-gorg-data nil))
    (set 'new-gorg-data (append gorg-data `(,gorg-entry)))))

(defun create-new-entry (gorg-data &optional attributes-list)
  "Accepts as arguments 1) an alist of gorg data, and optionally 2) an alist of attributes.  Returns an alist of gorg-data with a new entry affixed.  If an attributes list is passed in, it will become the attributes list of the new entry.  Otherwise, two attributes will be assigned: NAME, ' ', and BODY, ' '."
  (add-new-entry-to-data
   (create-new-entry-data 
    (create-new-entry-indexstring gorg-data) attributes-list)
   gorg-data))

;;;Writing Attributes;;;

;; (defun overwrite-association-to-alist (association existing-alist)
;;   "Accepts as arguments 1) a cons cell containing an association, and 2) an alist.  Will return an alist with 'association' modified"

;set gorg-directory
(set 'gorg-directory "/Users/lucasamodeonewman/Dropbox/Gorg/gorg-data")

;idioms for reading and writing
(set 'current-gorg-data (read-gorg-from-directory gorg-directory))
;; (write-gorg-to-directory current-gorg-data gorg-directory)	
current-gorg-data





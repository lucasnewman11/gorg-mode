;This file contains variable and face definitions which influence the behavior of gorg mode.

;CODE:

(provide 'gorg-interface-init)

;FACES
(defface gorg-headline
  '((t :foreground "blue"))
    "A face for top level gorg headlines.")

;SEGMENT TYPES
;(defvar segment-types
(set 'segment-types
     `(entry-headline 
       (stringfunc ,(lambda (gorg-entry-name)
		      "Accepts as argument A) a string containing the 'NAME' attribute of a gorg entry. Returns a properly formatted entry headline string created from that name."
		      (concat "* " gorg-entry-name))
	textprop (face gorg-headline))
       
       entry-body
       (stringfunc ,(lambda (gorg-entry-main)
		      "Accepts as argument A) a string containing the main of a gorg entry.  Returns a properly formatted entry body string created from that name."
		      gorg-entry-main)
        textprop ())
       
       entry-newline
       (stringfunc ,(lambda (nothing)
		      "Returns a new line character."
		      "\n")
	textprop (read-only t))))
;"A plist of segment types with their names.")

;INTERFACE TYPES
;(defvar interface-types
 
(set 'interface-types
    `(entry-node
       (segnames (headline-1 newline-1 body-1)
	segtypes (headline-1 entry-headline newline-1 entry-newline body-1 entry-body)
	bufferfun ,(lambda (gorg-entry)
		     "Accepts as argument A) a gorg entry.  Returns a newly created buffer object, '<name>.org', where <name> is the entry name attribute."
		     (require 'gorg-utilities)
		     (let ((entry-name nil))
		       (set 'entry-name (cdr (gorg-get-attribute-from-entry "NAME" gorg-entry)))
		       (generate-new-buffer (concat entry-name ".gorg"))))
		
     	segdatasfun ,(lambda (gorg-entry)
	      "Accepts as argument A) a gorg entry.  Returns a plist of segment constructor data, with the name of each segment paired with a gorg data subset."
	      (let ((entry-name nil)
		    (entry-main nil)
		    (segdatas ()))
		(set 'entry-name (cdr (gorg-get-attribute-from-entry "NAME" gorg-entry)))
		(set 'entry-main (cdr (gorg-get-attribute-from-entry "MAIN" gorg-entry)))
		(set 'segdatas (plist-put segdatas 'headline-1 entry-name))
		(set 'segdatas (plist-put segdatas 'body-1 entry-main))
		(set 'segdatas (plist-put segdatas 'newline-1 ""))
		(print segdatas)
		segdatas)))))
;"A plist of interface types with their names.")




	


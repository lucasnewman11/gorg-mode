;Functions that deal with reading from and writing to gorg directories.

;; By gorg directory, we mean a directory that contains many gorg files, each containing a JSON representation of gorg data:

;; Each file in a gorg directory should be titled "gorg-<data&time>.txt", where "<date&time>" is a 12 digit string of integers of the form YY-MM-DD-HH-MM-SS.  For example, "gorg-150826164430.txt" is a valid gorg file title, specifying a gorg file created on August 26th, 2015 at time 16:44 and 30 seconds.

;;CODE

(provide 'gorg-directory)

;;;READING FROM GORG DIRECTORIES;;;

(defun return-greatest-string-in-list (list-of-strings)
  "Accepts as argument A) a list of strings.  Uses the function 'string<' to return the largest string in A."
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

(defun gorg-directory-return-filepath-most-recent (gorg-directory-filepath-string)
  "Accepts as argument a A) string representation of a gorg directory filepath.  Returns the absolute filepath of the most recently created gorg file within that directory."
  (let ((files-in-directory nil))
    (set 'files-in-directory (directory-files gorg-directory-filepath-string))
    (concat gorg-directory-filepath-string "/" (return-greatest-string-in-list files-in-directory))
    ))

(defun gorg-read-data-from-directory (gorg-directory-filepath-string)
  "Accepts as argument A) a string representation of a gorg directory filepath.  Returns the entry list from the most recently created gorg file within taht directory."
  (require 'gorg-utilities)
  (gorg-read-data-from-filepath (gorg-directory-return-filepath-most-recent gorg-directory-filepath-string)))

;;;;;;;;WRITING TO GORG FILES;;;;;;;;

(defun create-gorg-filepath-to-write (directory-string)
  "Accepts as argument a string representation of a directory absolute file path.  Returns the absolute filepath of a new gorg file within that directory."
  (let ((filename . nil))
    (set 'filename (concat "gorg-" (format-time-string "%y%m%d%H%M%S") ".txt"))
    (set 'filepath (concat directory-string "/" filename))))


(defun write-gorg-to-directory (gorg-data directory-string)
  "Accepts as arguments 1) gorg data, and 2) a string containing a directory.  Writes a new gorg file to the directory."
  (require 'gorg-utilities)
  (gorg-write-data-to-filepath gorg-data (create-gorg-filepath-to-write directory-string)))


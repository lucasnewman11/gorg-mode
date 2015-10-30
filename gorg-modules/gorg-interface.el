;This file contains functions involving the creation and rendering of buffer segments and interfaces.

;CODE:
(provide 'gorg-interface)

(defun gorg-create-new-segment (gorg-data segment-type buffer)
  "Accepts as argument A) gorg data, B) a gorg segment type, and C) a buffer object.  Returns a gorg segment."
  (let ((segment ()))
    (set 'segment (plist-put segment 'string (funcall (plist-get segment-type 'stringfunc) gorg-data)))
    (set 'segment (plist-put segment 'start (make-marker)))
    (set 'segment (plist-put segment 'end (make-marker)))
    (set 'segment (plist-put segment 'properties (plist-get segment-type 'textprop)))
    (set 'segment (plist-put segment 'buffer buffer))
    segment))

(defun gorg-render-segment (segment)
  "Accepts as argument A) a segment.  Renders that segment."
  (print segment)
  (print (plist-get segment 'buffer))
  (set-buffer (plist-get segment 'buffer))
  (goto-char (plist-get segment 'start))
  (insert (plist-get segment 'string))
  (set-text-properties 
   (plist-get segment 'start) 
   (plist-get segment 'end) 
   (plist-get segment 'properties)))

(defun gorg-create-new-interface (gorg-data interface-type segment-types)
  "Accepts as argument A) gorg data, B) a gorg interface type, and C) a list of gorg segment types.  Returns a gorg interface, populated appropriately."
  (let ((segnames nil)
	(segtypes nil)
	(segdatas nil)
	(interface-buffer nil)
	(interface nil)
	(seg-index nil))
					; sets the list of segment names
    (set 'segnames (plist-get interface-type 'segnames)) 
					; sets the list of segment types
    (set 'segtypes (plist-get interface-type 'segtypes))
					; creates and sets the list of segment datas
    (set 'segdatas (funcall (plist-get interface-type 'segdatasfun) gorg-data))
					; creates and sets the interface buffer 
    (set 'interface-buffer (funcall (plist-get interface-type 'bufferfun) gorg-data))
					; creates the top level categories
    (set 'interface (plist-put interface 'segments '(1 1)))
    (set 'interface (plist-put interface 'order segnames))
					; creates the segments

    (dolist (segname segnames)
      (set 'segments (plist-put (plist-get interface 'segments) segname
		 (funcall 'gorg-create-new-segment 
			  (plist-get segdatas segname)
			  (plist-get segment-types (plist-get segtypes segname))
			  interface-buffer))))

    					;adds spaces in buffer
    (set-buffer interface-buffer)
    (goto-char 1)
    (dolist (segname segnames)
      (insert " "))

					;sets markers
    (set 'seg-index 1)
    (dolist (segname segnames)
      (set-marker
       (plist-get (plist-get (plist-get interface 'segments) segname) 'start)
       seg-index)
      (set-marker
       (plist-get (plist-get (plist-get interface 'segments) segname) 'end)
       seg-index)
      (set-marker-insertion-type 
       (plist-get (plist-get (plist-get interface 'segments) segname) 'end)
       t)
      (set 'seg-index (+ seg-index 1)))
					;returns the interface
    interface))

(defun gorg-render-interface (interface)
  "Accepts as argument A) a gorg interface.  Renders that interface by rendering each of its component segments in order."
  (let ((segments nil)
	(order nil))
      (set 'segments (plist-get interface 'segments))
      (set 'order (plist-get interface 'order))
      (dolist (segment order)
	(gorg-render-segment (plist-get segments segment)))))





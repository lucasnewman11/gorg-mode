(require 'gorg-directory)
(require 'gorg-utilities)
(require 'gorg-interface)
(require 'gorg-interface-init)

(set 'gorg-directory "/Users/lucasamodeonewman/Dropbox/Gorg/gorg-data/")
(set 'current-gorg-data (gorg-read-data-from-directory gorg-directory))


(progn
(load-file "gorg-interface-init.el")
(load-file "gorg-interface.el")
(set 'entry-node (plist-get interface-types 'entry-node))
(gorg-render-interface (gorg-create-new-interface (gorg-get-entry-from-entry-list "I0" current-gorg-data) entry-node segment-types)))









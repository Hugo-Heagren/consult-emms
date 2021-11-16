;;; consult-emms.el --- Consult interface to EMMS

;; Author: Hugo Heagren <hugo@heagren.com>
;; Version: 0.1
;; Package-Requires: ((consult) (emms))
;; Keywords: consult, emms

;;; Code:

(require 'consult)
(require 'emms)

(defgroup consult-emms nil
  "Customization group for consult-emms.")

;;;; Sources

(defcustom consult-emms-library-sources '(consult-emms--source-track)
  "Sources used by `consult-emms-library'.

See `consult--multi' for a description of the source values."
  :group 'consult-emms
  :type '(repeat symbol))

;;;;; Tracks

(defun consult-emms--get-tracks ()
  "Get list of EMMS tracks from `emms-cache-db'.

For each track, return a string with the track's name. This has a
property consult-emms--hash-key, with the track's hash key as its
value. The name defaults to \"unknown\" if it is not found."
  (mapcar (lambda (key)
	    (propertize
	     (or (assoc-default
		  'info-title
		  (gethash key emms-cache-db))
		 "unknown")
	     'consult-emms--hash-key key))
	  (hash-table-keys emms-cache-db)))

(defvar consult-emms--track-history ()
  "History of `consult-emms--source-track'.")

(defvar consult-emms--source-track
  `(:name     "Track"
    :narrow   ?t
    :category track
    :history  consult-emms--track-history
    :items    ,#'consult-emms--get-tracks
    :action   (lambda (trk-str) (emms-add-file (get-text-property 0 'consult-emms--hash-key trk-str))))
  "Track source for `consult-emms-library'.")

;;;; Entry Points

;;;###autoload
(defun consult-emms-library ()
  (interactive)
  (consult--multi consult-emms-library-sources
                 :require-match t
                 :prompt "EMMS Library: "
                 :history 'consult--buffer-history))

(provide 'consult-emms)

;;; consult-emms.el ends here

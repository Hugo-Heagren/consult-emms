;;; consult-emms-embark.el --- Embark actions for consult-emms

;; Author: Hugo Heagren <hugo@heagren.com>
;; Version: 0.1
;; Package-Requires: ((consult-emms) (embark))
;; Keywords: consult, emms, embark

;;; Code:

(require 'consult-emms)
(require 'embark)

;;;; Tracks

(defun consult-emms-embark--add-track-playlist (track-name)
  "Choose an EMMS playlist to add track TRACK-NAME to."
  (let* ((key (get-text-property 0 'consult-emms-track-key track-name))
	 (track (gethash key emms-cache-db))
	 (file (assoc-default 'name track nil nil))
	 (emms-playlist-buffer (consult-emms--choose-buffer)))
    (emms-add-file file)))

(embark-define-keymap consult-emms-embark-track-actions
  "Keymap for actions on tracks in `consult-emms'."
  ("p" '("Add to playlist" . consult-emms-embark--add-track-playlist)))

(add-to-list 'embark-keymap-alist '(track . consult-emms-embark-track-actions))

(provide 'consult-emms-embark)

;;; consult-emms-embark.el ends here

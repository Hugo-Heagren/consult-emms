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
  (let ((file (consult-emms--track-name-get track-name 'name))
	 (emms-playlist-buffer (consult-emms--choose-buffer)))
    (emms-add-file file)))

(defun consult-emms-embark--track-goto-album (track-name)
  "Select a track from the album to which TRACK-NAME belongs.

Selected track is added to the current playlist."
  (let* ((album (consult-emms--track-name-get track-name 'info-album))
	 (tracks (mapcar #'consult-emms--propertize-track-title
			 (consult-emms--get-album-tracks album)))
	 (track (consult--read tracks
			       :prompt (format "%s: " album)
			       :category 'track)))
    (consult-emms--add-track-current-playlist track)))

(defun consult-emms-embark--track-goto-artist (track-name)
  "Select a track by TRACK-NAME's artist.

Selected track is added to the current playlist."
  (let* ((artist (consult-emms--track-name-get track-name 'info-artist))
	 (tracks (mapcar #'consult-emms--propertize-track-title
			 (consult-emms--get-artist-tracks artist)))
	 (track (consult--read tracks
			       :prompt (format "%s: " artist)
			       :category 'track)))
    (consult-emms--add-track-current-playlist track)))

(defun consult-emms-embark--edit-track-tags (track-name)
  "Edit TRACK-NAME's tags in EMMS' tag editor."
  (let* ((key (get-text-property 0 'consult-emms-track-key track-name))
	 (track (gethash key emms-cache-db)))
    (emms-tag-editor-edit-track track)))

(embark-define-keymap consult-emms-embark-track-actions
  "Keymap for actions on tracks in `consult-emms'."
  ("p" '("Add to playlist" . consult-emms-embark--add-track-playlist))
  ("b" '("Goto album" . consult-emms-embark--track-goto-album))
  ("a" '("Goto artist" . consult-emms-embark--track-goto-artist))
  ("e" '("Edit tags" . consult-emms-embark--edit-track-tags)))

(add-to-list 'embark-keymap-alist '(track . consult-emms-embark-track-actions))

(provide 'consult-emms-embark)

;;; consult-emms-embark.el ends here

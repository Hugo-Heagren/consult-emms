;;; consult-emms-embark.el --- Embark actions for consult-emms

;; Author: Hugo Heagren <hugo@heagren.com>
;; Version: 0.1
;; Package-Requires: ((consult-emms) (embark))
;; Keywords: consult, emms, embark

;;; Code:

(require 'emms)
(require 'consult-emms)
(require 'embark)

;;;; Tracks

(defun consult-emms-embark--add-track-playlist (track-name)
  "Choose an EMMS playlist to add track TRACK-NAME to."
  (let ((file (consult-emms--track-name-get track-name 'name)))
    (consult-emms--with-chosen-current-playlist
     (emms-add-file file))))

(defun consult-emms-embark--track-goto-album (track-name)
  "Select a track from the album to which TRACK-NAME belongs.

Selected track is added to the current playlist."
  (let* ((album (consult-emms--track-name-get track-name 'info-album)))
    (consult-emms--choose-track-album album)))

(defun consult-emms-embark--track-goto-artist (track-name)
  "Select a track by TRACK-NAME's artist.

Selected track is added to the current playlist."
  (let* ((artist (consult-emms--track-name-get track-name 'info-artist)))
    (consult-emms--choose-track-artist artist)))

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

;;;; Albums

(defun consult-emms-embark--add-album-playlist (album-name)
  "Choose an EMMS playlist to add track ALBUM-NAME to."
  (consult-emms--with-chosen-current-playlist
   (consult-emms--add-album album-name)))

(defun consult-emms-embark--album-goto-artist (album)
  "Select a track by ALBUM's artist.

Selected track is added to the current playlist."
  ;; All the tracks will have the same album-artist, so we just check
  ;; the first one
  (let* ((any-track (car (consult-emms--get-album-tracks album)))
	 ;; If there is an explicit 'albumartist' tag, use that. If
	 ;; not (lots of files are not very well tagged), default to
	 ;; the artist of the song.
	 (artist (or
		  (emms-track-get (gethash any-track emms-cache-db) 'info-albumartist)
		  (emms-track-get (gethash any-track emms-cache-db) 'info-artist))))
    (consult-emms--choose-track-artist artist)))

(embark-define-keymap consult-emms-embark-album-actions
  "Keymap for actions on albums in `consult-emms'."
  ("p" '("Add to playlist". consult-emms-embark--add-album-playlist))
  ("b" '("View album" . consult-emms--choose-track-album))
  ("a" '("Goto artist" . consult-emms-embark--album-goto-artist)))

(add-to-list 'embark-keymap-alist '(album . consult-emms-embark-album-actions))

;;;; Artists

(defun consult-emms-embark--add-artist-playlist (artist-name)
  "Choose an EMMS playlist to add track ARTIST-NAME to."
  (consult-emms--with-chosen-current-playlist
   (consult-emms--add-artist artist-name)))

(embark-define-keymap consult-emms-embark-artist-actions
  "Keymap for actions on artists in `consult-emms'."
  ("p" '("Add to playlist". consult-emms-embark--add-artist-playlist)))

(add-to-list 'embark-keymap-alist '(artist . consult-emms-embark-artist-actions))

;;;; Playlists

(defun consult-emms-embark--get-buffer-text-property (playlist-name)
  "Return text property 'consult-emms--buffer of PLAYLIST-NAME."
  (get-text-property 0 'consult-emms--buffer playlist-name))

(defun consult-emms-embark--with-buffer-from-text-property (playlist-name &rest body)
  "Execute BODY with playlist from PLAYLIST-NAME as current."
  (consult-emms--with-current-playlist
   (consult-emms-embark--get-buffer-text-property playlist-name) body))

(defun consult-emms-embark--write-playlist (playlist-name)
  "Write PLAYLIST-NAME to file."
  (consult-emms-embark--with-buffer-from-text-property
   playlist-name (call-interactively 'emms-playlist-save)))

(defun consult-emms-embark--kill-playlist (playlist-name)
  "Kill playlist buffer extracted from PLAYLIST-NAME."
  (kill-buffer
   (consult-emms-embark--get-buffer-text-property playlist-name)))

(defun consult-emms-embark--clear-playlist (playlist-name)
  "Clear playlist extracted from PLAYLIST-NAME."
  (consult-emms-embark--with-buffer-from-text-property
   playlist-name (emms-playlist-clear)))

(defun consult-emms-embark--shuffle-playlist (playlist-name)
  "Shuffle playlist extracted from PLAYLIST-NAME."
  (consult-emms-embark--with-buffer-from-text-property
   playlist-name (emms-shuffle)))

(defun consult-emms-embark--rename-playlist (playlist-name)
  "Rename playlist extracted from PLAYLIST-NAME."
  (let ((buffer
	 (consult-emms-embark--get-buffer-text-property playlist-name)))
    (with-current-buffer buffer
      (call-interactively 'rename-buffer))))

(embark-define-keymap consult-emms-embark-playlist-actions
  "Keymap for actions on playlists in `consult-emms'."
  ("W" '("Write to file" . consult-emms-embark--write-playlist))
  ("k" '("Kill playlist" . consult-emms-embark--kill-playlist))
  ("c" '("Clear playlist" . consult-emms-embark--clear-playlist))
  ("s" '("Shuffle playlist" . consult-emms-embark--shuffle-playlist)))

(add-to-list 'embark-keymap-alist '(playlist . consult-emms-embark-playlist-actions))

(provide 'consult-emms-embark)

;;; consult-emms-embark.el ends here

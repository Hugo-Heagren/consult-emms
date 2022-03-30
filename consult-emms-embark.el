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

(defun consult-emms-embark--track-add-album (track-name)
  "Add album to which TRACK-NAME belongs to current playlist."
  (let* ((album (consult-emms--track-name-get track-name 'info-album)))
    (consult-emms--add-album album)))

(defun consult-emms-embark--edit-track-tags (track-name)
  "Edit TRACK-NAME's tags in EMMS' tag editor."
  (let* ((key (get-text-property 0 'consult-emms-track-key track-name))
	 (track (gethash key emms-cache-db)))
    (emms-tag-editor-edit-track track)))

(embark-define-keymap consult-emms-embark-track-actions
  "Keymap for actions on tracks in `consult-emms'."
  ("p" '("Add to playlist" . consult-emms-embark--add-track-playlist))
  ("g" '("Goto..." . consult-emms-embark-track-goto))
  ("e" '("Edit tags" . consult-emms-embark--edit-track-tags)))

(defvar consult-emms-embark-track-goto
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" '("Artist" . consult-emms-embark--track-goto-artist))
    (keymap-set map "b" '("Album" . consult-emms-embark--track-goto-album))
    map)
  "Keymap for actions moving from a track to an associated entity.")
(fset 'consult-emms-embark-track-goto consult-emms-embark-track-goto)

(add-to-list 'embark-keymap-alist '(track . consult-emms-embark-track-actions))

;;;;; Playlist Tracks
;; (tracks which have a position on a playlist)

(defun consult-emms-embark--kill-playlist-track (track-name)
  (consult-emms--do-playlist-track
   track-name (emms-playlist-mode-kill-entire-track)))

(embark-define-keymap consult-emms-embark-playlist-track-actions
  "Keymap for actions on tracks in playlists in `consult-emms'."
  :parent consult-emms-embark-track-actions
  ("k" '("Kill track" . consult-emms-embark--kill-playlist-track)))

(add-to-list 'embark-keymap-alist '(playlist-track . consult-emms-embark-playlist-track-actions))

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
  ("g" '("Goto..." . consult-emms-embark-album-goto)))

(defvar consult-emms-embark-album-goto
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" '("Artist" . consult-emms-embark--album-goto-artist))
    (keymap-set map "b" '("Album" . consult-emms--choose-track-album))
    map)
  "Keymap for actions moving from a track to an associated entity.")
(fset 'consult-emms-embark-album-goto consult-emms-embark-album-goto)

(add-to-list 'embark-keymap-alist '(album . consult-emms-embark-album-actions))

;;;; Artists

(defun consult-emms-embark--add-artist-playlist (artist-name)
  "Choose an EMMS playlist to add track ARTIST-NAME to."
  (consult-emms--with-chosen-current-playlist
   (consult-emms--add-artist artist-name)))

(embark-define-keymap consult-emms-embark-artist-actions
  "Keymap for actions on artists in `consult-emms'."
  ("p" '("Add to playlist". consult-emms-embark--add-artist-playlist))
  ("g" '("Goto...". consult-emms-embark-artist-goto)))

(defvar consult-emms-embark-artist-goto
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" '("Artist" . consult-emms--choose-track-artist))
    map)
  "Keymap for actions moving from a track to an associated entity.")
(fset 'consult-emms-embark-artist-goto consult-emms-embark-artist-goto)

(add-to-list 'embark-keymap-alist '(artist . consult-emms-embark-artist-actions))

;;;; Playlists

(defun consult-emms-embark--write-playlist (playlist)
  "Write PLAYLIST to a file (prompts for filename)."
  ;; If the playlist is the current buffer, EMMS won't raise
  ;; exceptions about the buffer not being current (which the user
  ;; likely knows already if they are using `consult-emms'!)
  (with-current-buffer playlist
    (consult-emms--with-current-playlist
     playlist (call-interactively 'emms-playlist-save))))

(defun consult-emms-embark--clear-playlist (playlist-name)
  "Clear playlist in buffer PLAYLIST-NAME."
  (with-current-buffer playlist-name
    (emms-playlist-clear)))

(defun consult-emms-embark--shuffle-playlist (playlist-name)
  "Shuffle playlist in buffer PLAYLIST-NAME."
  (with-current-buffer playlist-name (emms-shuffle)))

(defun consult-emms-embark--insert-playlist (playlist-name)
  "Append playlist in buffer PLAYLIST-NAME to another playlist."
  (let ((new-playlist (consult-emms--choose-buffer)))
    (with-current-buffer new-playlist
      (let ((inhibit-read-only t))
	(save-excursion
	  (goto-char (point-max))
	  (insert-buffer playlist-name))))))

(defun consult-emms-embark--playlist-set-active (playlist-name)
  "Make buffer PLAYLIST-NAME the current/active EMMS playlist."
  (emms-playlist-set-playlist-buffer playlist-name))

(embark-define-keymap consult-emms-embark-playlist-actions
  "Keymap for actions on playlists in `consult-emms'."
  :parent embark-buffer-map
  ("W" '("Write to file" . consult-emms-embark--write-playlist))
  ("c" '("Clear playlist" . consult-emms-embark--clear-playlist))
  ("s" '("Shuffle playlist" . consult-emms-embark--shuffle-playlist))
  ("i" '("Insert into playlist" . consult-emms-embark--insert-playlist))
  ("a" '("Make active/current playlist" . consult-emms-embark--playlist-set-active)))

(add-to-list 'embark-keymap-alist '(playlist . consult-emms-embark-playlist-actions))

;;;; Streams

(defun consult-emms-embark--add-stream-playlist (stream-name)
  "Choose an EMMS playlist to add track STREAM-NAME to."
  (consult-emms--with-chosen-current-playlist
   (consult-emms--add-stream stream-name)))

(embark-define-keymap consult-emms-embark-stream-actions
  "Keymap for actions on streams in `consult-emms'."
  ("p" '("Add to playlist" . consult-emms-embark--add-stream-playlist)))

(add-to-list 'embark-keymap-alist '(stream . consult-emms-embark-stream-actions))

(provide 'consult-emms-embark)

;;; consult-emms-embark.el ends here

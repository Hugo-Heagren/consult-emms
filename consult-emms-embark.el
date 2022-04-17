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
    (consult-emms--choose-track-or-album-artist artist)))

(defun consult-emms-embark--track-goto-genre (track-name)
  "Select a track from the genre to which TRACK-NAME belongs.

Selected track is added to the current playlist."
  (let* ((genre (consult-emms--track-name-get track-name 'info-genre)))
    (consult-emms--choose-track-or-album-genre genre)))

(defun consult-emms-embark--track-add-album (track-name)
  "Add album to which TRACK-NAME belongs to current playlist."
  (let* ((album (consult-emms--track-name-get track-name 'info-album)))
    (consult-emms--add-album album)))

(defun consult-emms-embark--track-add-artist (track-name)
  "Add TRACK-NAME's artist to current playlist."
  (let* ((artist (consult-emms--track-name-get track-name 'info-artist)))
    (consult-emms--add-artist artist)))

(defun consult-emms-embark--track-add-genre (track-name)
  "Add TRACK-NAME's genre to current playlist."
  (let* ((genre (consult-emms--track-name-get track-name 'info-genre)))
    (consult-emms--add-genre genre)))

(defun consult-emms-embark--edit-track-tags (track-name)
  "Edit TRACK-NAME's tags in EMMS' tag editor."
  (let* ((key (get-text-property 0 'consult-emms-track-key track-name))
	 (track (gethash key emms-cache-db)))
    (emms-tag-editor-edit-track track)))

(embark-define-keymap consult-emms-embark-track-actions
  "Keymap for actions on tracks in `consult-emms'."
  ("p" '("Add to playlist" . consult-emms-embark--add-track-playlist))
  ("g" '("Goto..." . consult-emms-embark-track-goto))
  ("a" '("Add... " . consult-emms-embark-track-add))
  ("e" '("Edit tags" . consult-emms-embark--edit-track-tags)))

(defvar consult-emms-embark-track-goto
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" '("Artist" . consult-emms-embark--track-goto-artist))
    (keymap-set map "b" '("Album" . consult-emms-embark--track-goto-album))
    (keymap-set map "g" '("Genre" . consult-emms-embark--track-goto-genre))
    map)
  "Keymap for actions moving from a track to an associated entity.")
(fset 'consult-emms-embark-track-goto consult-emms-embark-track-goto)

(defvar consult-emms-embark-track-add
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" '("Artist" . consult-emms-embark--track-add-artist))
    (keymap-set map "b" '("Album" . consult-emms-embark--track-add-album))
    (keymap-set map "g" '("Genre" . consult-emms-embark--track-add-genre))
    map)
  "Keymap for actions queuing track-associated entities.")
(fset 'consult-emms-embark-track-add consult-emms-embark-track-add)

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
  "Choose an EMMS playlist to add album ALBUM-NAME to."
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
    (consult-emms--choose-track-or-album-artist artist)))

(defun consult-emms-embark--album-goto-genre (album)
  "Select a track or album in ALBUM's genre.

The first song in ALBUM is examined. Selection is added to the
current playlist."
  (let* ((any-track (car (consult-emms--get-album-tracks album)))
	 (genre (emms-track-get (gethash any-track emms-cache-db) 'info-genre)))
    (consult-emms--choose-track-or-album-genre genre)))

;; NOTE This blatantly copies the structure of the above function, but
;; two uses really enough to justify abstracting it out.
(defun consult-emms-embark--album-add-artist (album)
  "Add all tracks by ALBUM's artist to current playlist.

The first song in ALBUM is examined. If it has an `albumartist'
tag, that value is used, otherwise use the value of `artist'."
  ;; All the tracks will have the same album-artist, so we just check
  ;; the first one
  (let* ((any-track (car (consult-emms--get-album-tracks album)))
	 ;; If there is an explicit 'albumartist' tag, use that. If
	 ;; not (lots of files are not very well tagged), default to
	 ;; the artist of the song.
	 (artist (or
		  (emms-track-get (gethash any-track emms-cache-db) 'info-albumartist)
		  (emms-track-get (gethash any-track emms-cache-db) 'info-artist))))
    (consult-emms--add-artist artist)))

(defun consult-emms-embark--album-add-genre (album)
  "Add all tracks of ALBUM's genre to current playlist.

The first song in ALBUM is examined for its `genre' tag."
  ;; Tracks in albums generally have the same genre, so just assume
  ;; the first track is a good guide. If the user wants the genre of a
  ;; SPECIFIC track, they can just navigate to that track and use the
  ;; genre controls from there.
  (let* ((any-track (car (consult-emms--get-album-tracks album)))
	 (genre (emms-track-get (gethash any-track emms-cache-db) 'info-genre)))
    (consult-emms--add-genre genre)))

(embark-define-keymap consult-emms-embark-album-actions
  "Keymap for actions on albums in `consult-emms'."
  ("p" '("Add to playlist". consult-emms-embark--add-album-playlist))
  ("g" '("Goto..." . consult-emms-embark-album-goto))
  ("a" '("Add..." . consult-emms-embark-album-add)))

(defvar consult-emms-embark-album-goto
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" '("Artist" . consult-emms-embark--album-goto-artist))
    (keymap-set map "b" '("Album" . consult-emms--choose-track-album))
    (keymap-set map "g" '("Genre" . consult-emms-embark--album-goto-genre))
    map)
  "Keymap for actions moving from a track to an associated entity.")
(fset 'consult-emms-embark-album-goto consult-emms-embark-album-goto)

(defvar consult-emms-embark-album-add
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" '("Artist" . consult-emms-embark--album-add-artist))
    (keymap-set map "g" '("Genre" . consult-emms-embark--album-add-genre))
    map)
  "Keymap for actions queuing an album-associated entity.")
(fset 'consult-emms-embark-album-add consult-emms-embark-album-add)

(add-to-list 'embark-keymap-alist '(album . consult-emms-embark-album-actions))

;;;; Artists

(defun consult-emms-embark--add-artist-playlist (artist-name)
  "Choose an EMMS playlist to add artist ARTIST-NAME to."
  (consult-emms--with-chosen-current-playlist
   (consult-emms--add-artist artist-name)))

(embark-define-keymap consult-emms-embark-artist-actions
  "Keymap for actions on artists in `consult-emms'."
  ("p" '("Add to playlist". consult-emms-embark--add-artist-playlist))
  ("g" '("Goto...". consult-emms-embark-artist-goto)))

(defvar consult-emms-embark-artist-goto
  (let ((map (make-sparse-keymap)))
    (keymap-set map "a" '("Artist" . consult-emms--choose-track-or-album-artist))
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

;;;; Genre

(defun consult-emms-embark--add-genre-playlist (genre-name)
  "Choose an EMMS playlist to add genre GENRE-NAME to."
  (consult-emms--with-chosen-current-playlist
   (consult-emms--add-genre genre-name)))

(defun consult-emms--choose-track-or-album-genre (genre)
  "Choose a track or album from those in GENRE.

The selected item is added to the current playlist.

The two lists are presented with `consult--multi'. The track list
is built with `consult-emms--get-genre-tracks', and the album
list is generated by extracting the album names of each track in
the track list."
  (let* (;; Tracks
	 (tracks-list
	  (mapcar #'consult-emms--propertize-track-title
		  (consult-emms--get-genre-tracks genre)))
	 (tracks-source (plist-put
			 (purecopy consult-emms--source-track)
			 :items tracks-list))
	 ;; Albums
	 (albums-list
	  (mapcar (lambda (trk) (consult-emms--track-name-get trk 'info-album))
		  tracks-list))
	 (albums-source (plist-put
			 (purecopy consult-emms--source-album)
			 :items albums-list)))
    (consult--multi `(,tracks-source ,albums-source)
		    :require-match t
		    :prompt (format "%s: " genre))))

(embark-define-keymap consult-emms-embark-genre-actions
  "Keymap for actions on genres in `consult-emms'."
  ("p" '("Add to playlist". consult-emms-embark--add-genre-playlist))
  ("g" '("Goto...". consult-emms-embark-genre-goto)))

(defvar consult-emms-embark-genre-goto
  (let ((map (make-sparse-keymap)))
    (keymap-set map "g" '("Genre" . consult-emms--choose-track-or-album-genre))
    map)
  "Keymap for actions moving from a track to an associated entity.")
(fset 'consult-emms-embark-genre-goto consult-emms-embark-genre-goto)

(add-to-list 'embark-keymap-alist '(genre . consult-emms-embark-genre-actions))

;;;; Streams

(defun consult-emms-embark--add-stream-playlist (stream-name)
  "Choose an EMMS playlist to add stream STREAM-NAME to."
  (consult-emms--with-chosen-current-playlist
   (consult-emms--add-stream stream-name)))

(embark-define-keymap consult-emms-embark-stream-actions
  "Keymap for actions on streams in `consult-emms'."
  ("p" '("Add to playlist" . consult-emms-embark--add-stream-playlist)))

(add-to-list 'embark-keymap-alist '(stream . consult-emms-embark-stream-actions))

;;;; EMMS Buffer Embark Targets

(defun consult-emms-embark-identify-music-at-point ()
  "Identify musical object at point and its type.

If an EMMS track, artist or album is at point, return a list of
the form (TYPE ID BEG . END), where:
- TYPE is the relevant completion type, as recognised by embark and
  consult ('album, 'track, etc.)
- ID is an identifier for the thing, formatted appropriately for its
  type
- BEG is the position of the BOL
- END is the position of the EOL."
  (when-let ((data (emms-browser-bdata-at-point))
	     (type-indicator
	      (assoc-default 'type data))
	     (type (assoc-default
		    type-indicator
		    '((info-title . track)
		      (info-album . album)
		      (info-artist . artist))))
	     (str (pcase type
		    ('track (consult-emms--propertize-track-title
			     (assoc-default
			      'name
			      (car (assoc-default 'data data)))))
		    ((or album artist) (assoc-default 'name data)))))
    `(,type ,str ,(line-beginning-position) . ,(line-end-position))))

(add-to-list
 'embark-target-finders
 'consult-emms-embark-identify-music-at-point)

(defun consult-emms-embark-identify-playlist-at-point ()
  "Identify EMMS playlist at point.

In an EMMS metaplaylist buffer, if there is a name of a buffer at
point, return a list `(playlist BUFFER-NAME BEG END)' where:
- BUFFER-NAME is the name of the buffer
- BEG is the position of the BOL
- END is the position of the EOL."
  (when-let* (((eq major-mode 'emms-metaplaylist-mode))
	      (bol (line-beginning-position))
	      (eol (line-end-position))
	      (buffer-name (buffer-substring bol eol))
	      ;; This effectively acts a check to see if there really
	      ;; IS a buffer with the name `buffer-name'
	      ((get-buffer buffer-name)))
    `(playlist ,buffer-name ,bol . ,eol)))

(add-to-list
 'embark-target-finders
 'consult-emms-embark-identify-playlist-at-point)

(provide 'consult-emms-embark)

;;; consult-emms-embark.el ends here

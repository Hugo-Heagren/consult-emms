;;; consult-emms.el --- Consult interface to EMMS

;; Author: Hugo Heagren <hugo@heagren.com>
;; Version: 0.1
;; Package-Requires: ((consult) (emms))
;; Keywords: consult, emms

;;; Code:

(require 'consult)
(require 'emms)
(require 'cl-lib)

;;;; Variables

(defgroup consult-emms nil
  "Customization group for consult-emms.")

;;;; Utils

(defun consult-emms--gen-history-var-name (thing)
  "Generate the name of a history variable for THING.

This will have the form `consult-emms--THING-history', returned
as a symbol. THINGS can be a string or a symbol."
  (intern (format "consult-emms--%s-history"
		  (if (symbolp thing)
		      (symbol-name thing)
		    thing))))

(defun consult-emms--track-name-get (track-name name &optional default)
  "Return the value of NAME property of track TRACK-NAME.

TRACK-NAME should be a string with the property
consult-emms-track-key. If DEFAULT is non-nil and the track does
not specify NAME, return DEFAULT."
  (let* ((key (get-text-property 0 'consult-emms-track-key track-name))
	 (track (gethash key emms-cache-db)))
    (assoc-default name track nil default)))

(defun consult-emms--choose-track-artist (artist)
  "Choose a track from those by ARTIST.

The selected track is added to the current playlist."
  (let* ((tracks (mapcar #'consult-emms--propertize-track-title
			 (consult-emms--get-artist-tracks artist)))
	 (track (consult--read tracks
			       :prompt (format "%s: " artist)
			       :category 'track)))
    (consult-emms--add-track-current-playlist track)))

;;;; Sources

(defcustom consult-emms-library-sources '(consult-emms--source-track
					  consult-emms--source-album
					  consult-emms--source-artist)
  "Sources used by `consult-emms-library'.

See `consult--multi' for a description of the source values. The
only exception is :cache. If non-nil, the source's will use cache
values between uses. This is useful for sources which need to do
complex operations on many candidates (like for albums)."
  :group 'consult-emms
  :type '(repeat symbol))

(cl-defmacro consult-emms--def-library-source (base &rest keys
						    &key items action
						    &allow-other-keys)
  "Define a source for `consult-emms-library'.

Given BASE, create a two variables:
consult-emms--BASE-history (initialised as nil) and
consult-emms--source-BASE. The latter is a plist with the
following keys and values:

- :name - capitalised version of string of BASE.
- :category - BASE.
- :history - symbol of the above history variable.
- :items - ITEMS (required), handled by `consult--multi'.
- :action - ACTION (required), handled by `consult--multi'.
- :cache - whether to do caching. Defaults to t. Decision is made with
  `if', so any non-nil value means to do caching.

All keys set automatically (e.g. :history) can be overridden by
passing the key as an argument directly.

Any other keys passed (e.g. :narrow) will be included in
consult-emms--source-BASE, which is passed as a source to
`consult--multi', which see."
  ;; Requirements
  (unless items  (error "Items not specified."))
  (unless action (error "Action not specified."))
  (let* ((name-string (symbol-name base))
	 (src-var-name (intern (concat "consult-emms--source-" name-string)))
	 (hist-var-name (intern (concat "consult-emms--" name-string "-history")))
	 (def-prop-list (list
		     :name (capitalize name-string)
    		     :category base
    		     :history hist-var-name
    		     :items items
    		     :action action
    		     :cache t)))
    (list 'progn
	  ;; Def history variable
	  `(defvar ,hist-var-name nil
	     ,(concat "History of `" (symbol-name src-var-name) "'."))
	  ;; Create correct options list. Anything specified in the
	  ;; call takes precedence, default to values in above list.
	  (cl-loop for (prop val) on def-prop-list by 'cddr
		   do (unless (plist-member keys prop)
			(plist-put keys prop val)))
	  ;; Def source variable
	  `(defvar ,src-var-name
	     (quote ,keys)
	     ;; Docstring
	     ,(concat (capitalize name-string) " source for `consult-emms-library'.")))))

;;;;; Tracks

(defun consult-emms--propertize-track-title (track-key)
  "Return the title of TRACK-KEY's track, propertized with TRACK-KEY.

TRACK-KEY is a key in `emms-cache-db'. Returns the title of the
track that key points to (or \"unknown\" if the title can't be
retrieved), with TRACK-KEY as the value of a property
consult-emms-track-key."
  (propertize
   (or (assoc-default
	'info-title
	(gethash track-key emms-cache-db))
       "unknown")
   'consult-emms-track-key track-key))

(defun consult-emms--get-tracks ()
  "Get list of EMMS tracks from `emms-cache-db'.

For each track, return a string with the track's name. This has a
property consult-emms-track-key, with the track's hash key as its
value. The name defaults to \"unknown\" if it is not found."
  (mapcar #'consult-emms--propertize-track-title
	  (hash-table-keys emms-cache-db)))

(defun consult-emms--add-track-current-playlist (track-name)
  "Add track TRACK-NAME to current playlist.

TRACK-NAME should be a propertized string, from which a key in
`emms-cache-db' can be extracted."
  (emms-add-file (consult-emms--track-name-get track-name 'name)))

(consult-emms--def-library-source track
				  :items    consult-emms--get-tracks
				  :action   #'consult-emms--add-track-current-playlist
				  :narrow   ?t)

;;;;; Albums

(defvar consult-emms--album-cache (make-hash-table :test #'equal)
  "Hash table caching albums for `consult-emms--source-album'.")

(defun consult-emms--album-cache-reset ()
  "Populate `consult-emms--album-cache'.

Each key is an album name (as a string), each value is a list of
keys in `emms-cache-db' for the tracks in that album. Returns the
list of album names."
  (let ((albums '()))
    (maphash
     (lambda (key value) (if-let ((album (assoc-default 'info-album value nil nil)))
			(progn (puthash album
					(append (list key) (gethash album consult-emms--album-cache))
					consult-emms--album-cache)
			       (setq albums (append (list album) albums)))))
     emms-cache-db)
    (delete-dups albums)))

(defun consult-emms--get-albums ()
  "Return a list of albums in `emms-cache-db'.

Specifically, if caching is disabled for
`consult-emms--source-album', or `consult-emms--album-cache' is
empty and `emms-cache-db' is not empty, then rebuild the cache
with `consult-emms--album-cache-reset'. (The second situation
covers the first invocation in a new session.) Then whether the
cache was rebuilt or not, return a list of keys for
`consult-emms--album-cache'."
  (or (when (or (not (plist-get consult-emms--source-album :cache)) ;; Caching disabled
		(and (not (hash-table-empty-p emms-cache-db)) ;; the emms cache is non-empty and...
			  (hash-table-empty-p consult-emms--album-cache))) ;; ...the cache var is empty
	(consult-emms--album-cache-reset)) ;; Update the cache (this returns the new list of keys)
      ;; Don't need to update, just return the keys
      (hash-table-keys consult-emms--album-cache)))

(defun consult-emms--guess-track-number (track)
  "Guess the track number of TRACK.

If TRACK includes `info-tracknumber' metadata, return that as an
integer. Otherwise Attempt to parse an integer from the beginning
of it's `name' metadata. If neither of these succeeds, return
nil."
  (string-to-number
   (or
    (assoc-default 'info-tracknumber track nil nil)
    (file-name-base (assoc-default 'name track nil nil)))))

(defun consult-emms--compare-track-numbers (a b)
  "Return t if track with key A has a lower tracknumber than B.

Tracknumbers are fetched with `consult-emms--guess-track-number'."
  (< (consult-emms--guess-track-number (gethash a emms-cache-db))
     (consult-emms--guess-track-number (gethash b emms-cache-db))))

(defun consult-emms--get-album-tracks (album)
  "Return tracks in ALBUM, a key in `consult-emms--album-cache'.

Returns a list of keys in `emms-cache-db'. These are sorted by
their track number, compared with
`consult-emms--compare-track-numbers'."
  (sort (gethash album consult-emms--album-cache)
	'consult-emms--compare-track-numbers))

(defun consult-emms--add-album (album)
  (mapcar (lambda (trk)
	    (emms-add-file (assoc-default 'name (gethash trk emms-cache-db) nil nil)))
	  (consult-emms--get-album-tracks album)))

(consult-emms--def-library-source album
				  :narrow ?b
				  :items  consult-emms--get-albums
				  :action consult-emms--add-album)

;;;;; Artists

(defvar consult-emms--artist-cache (make-hash-table :test #'equal)
  "Hash table caching artists for `consult-emms--source-artist'.")

(defun consult-emms--artist-cache-reset ()
  "Populate `consult-emms--artist-cache'.

Each key is an artist name (as a string), each value is a list of
keys in `emms-cache-db' for the tracks by that artist. Returns the
list of artist names."
  (let ((artists '()))
    (maphash
     (lambda (key value) (if-let ((artist (assoc-default 'info-artist value nil nil)))
			(progn (puthash artist
					(append (list key) (gethash artist consult-emms--artist-cache))
					consult-emms--artist-cache)
			       (setq artists (append (list artist) artists)))))
     emms-cache-db)
    (delete-dups artists)))

(defun consult-emms--get-artists ()
  "Return a list of artists in `emms-cache-db'.

Specifically, if caching is disabled for
`consult-emms--source-artist', or `consult-emms--artist-cache' is
empty and `emms-cache-db' is not empty, then rebuild the cache
with `consult-emms--artist-cache-reset'. (The second situation
covers the first invocation in a new session.) Then whether the
cache was rebuilt or not, return a list of keys for
`consult-emms--artist-cache'."
  (or (when (or (not (plist-get consult-emms--source-artist :cache)) ;; Caching disabled
		(and (not (hash-table-empty-p emms-cache-db)) ;; the emms cache is non-empty and...
			  (hash-table-empty-p consult-emms--artist-cache))) ;; ...the cache var is empty
	(consult-emms--artist-cache-reset)) ;; Update the cache (this returns the new list of keys)
      ;; Don't need to update, just return the keys
      (hash-table-keys consult-emms--artist-cache)))

(defun consult-emms--get-artist-tracks (artist)
  "Return tracks in ARTIST, a key in `consult-emms--artist-cache'.

Returns a list of keys in `emms-cache-db'."
  (gethash artist consult-emms--artist-cache))

(defun consult-emms--add-artist (artist)
  (mapcar (lambda (trk)
	    (emms-add-file (assoc-default 'name (gethash trk emms-cache-db) nil nil)))
	  (consult-emms--get-artist-tracks artist)))

(consult-emms--def-library-source artist
				  :narrow ?a
				  :items  consult-emms--get-artists
				  :action consult-emms--add-artist)

;;;;; Streams

(defvar consult-emms--stream-cache (make-hash-table :test #'equal)
  "Hash table caching streams for `consult-emms--source-stream'.")

(defun consult-emms--get-streams ()
  "Get list of EMMS tracks from ‘emms-cache-db’.

For each track, return a string with the stream's name. This has
a property `consult-emms-stream-url' with the stream's url as
its value. The name defaults to \"unknown\" if it is not found."
  (let* ((file-streams-list (with-temp-buffer
			      (emms-insert-file-contents emms-streams-file)
			      (goto-char (point-min))
			      (when (not (emms-source-playlist-native-p))
				(error "Cannot parse `emms-streams-file.'"))
			      (emms-source-playlist-parse-native nil)))
	 (streams (delete-dups (append file-streams-list
				       emms-streams-built-in-list))))
    ;; Map over the list of streams. For each:
    ;; - first item of metadata is the name
    ;; - second item of metadata is the url
    ;; - set key=name,value=url in the cache map
    (mapcar (lambda (stream)
	      (if-let* ((md (assoc-default 'metadata stream nil nil))
			(name (or (car md) "unknown"))
			(url (cadr md)))
		  (propertize name 'consult-emms-stream-url url)))
	    streams)))

(defun consult-emms--add-stream (stream)
  "Insert STREAM into the current EMMS buffer, and play it."
  (emms-add-streamlist
   (get-text-property 0 'consult-emms-stream-url stream))
  (with-current-emms-playlist
    (emms-playlist-last)
    (emms-playlist-mode-play-smart)))

(consult-emms--def-library-source stream
				  :narrow ?s
				  :items  consult-emms--get-streams
				  :action consult-emms--add-stream)

;;;; Playlists

(defun consult-emms--get-tracks-playlist-buffer (buffer)
  "Get a list of tracks in an EMMS BUFFER.

Returns a list of strings, each the title of a track. Each string
has two properties. The value of `consult-emms-track-key' is the
hash key of the relevant track in `emms-cache-db'. The value of
`consult-emms-track-pos' is the char position in at the beginning
of the tracks's line in BUFFER."
   (with-current-buffer buffer
     (save-excursion
       (cl-loop initially do (goto-char (point-min))
		until (eobp)
		if (emms-playlist-track-at)
		collect (let* ((trk (emms-playlist-track-at))
			       (title (emms-track-get trk 'info-title))
			       (key (emms-track-get trk 'name)))
			  (propertize title
				      'consult-emms-track-key key
				      'consult-emms-track-pos (point)))
		and
		do (forward-line)
		end))))

(defun consult-emms--play-track-by-pos (buffer pos)
  "Play track at POS in emms playlist buffer BUFFER."
  (with-current-buffer buffer
    (goto-char pos)
    (emms-playlist-mode-play-current-track)))

(defun consult-emms--playlist-source-from-buffer (buffer)
  "Make a source for `consult-emms-playlists' from BUFFER."
  (let* ((name (buffer-name buffer))
	 (hist-sym (intern (concat "consult-emms--" name "-buffer-history"))))
    `(:items ,(consult-emms--get-tracks-playlist-buffer buffer)
      :category track
      :name ,name
      :sort nil
      :action (lambda (str) (consult-emms--play-track-by-pos
			       ,buffer (get-text-property 0 'consult-emms-track-pos str))))))

(defun consult-emms--lookup-playlist-pos (_ candidates cand)
  "Lookup CAND in CANDIDATES list and return property 'consult--candidate."
  (when-let (found (member cand candidates))
    (get-text-property 0 'consult-emms-track-pos (car found))))

(defun consult-emms--playlist (buffer)
  "Select a track from EMMS buffer BUFFER."
  ;; `consult-emms--playlist-source-from-buffer' does most of the work
  ;; of forming the args for us, and it's a good idea to avoid code
  ;; duplication, so we use it here. BUT, it forms a source for
  ;; `consult--multi', which is different from the arg list taken by
  ;; `consult--read', so we have to transform it a bit.
  (let* ((raw-args (consult-emms--playlist-source-from-buffer buffer))
	 (items (plist-get raw-args :items))
	 ;; TODO Get this list programatically
	 (allowed '(:prompt :predicate :require-match ;; Keywords in `consult--read'
		    :history :default :keymap
		    :category :initial :narrow
		    :add-history :annotate :state
		    :preview-key :sort :lookup
		    :group :inherit-input-method))
	 ;; Use only arg keys used by `consult--read'
	 (filtered-args (cl-loop for (key value) on raw-args by 'cddr
				 if (member key allowed)
				 collect key and collect value))
	 (read-args (append `(:prompt ,(format "EMMS playlist <%s>: " (string-trim (buffer-name buffer)))
			      :lookup consult-emms--lookup-playlist-pos)
			    filtered-args))
	 (track (apply 'consult--read `(,items ,@read-args))))
    (consult-emms--play-track-by-pos buffer track)))

(defun consult-emms--choose-buffer ()
  "Choose one of the currently open EMMS playlists.

Returns the buffer object. The list if fetched with
`emms-metaplaylist-mode-sorted-buffer-list'."
  (let* ((playlist-list (mapcar (lambda (buffer) `(,(string-trim (buffer-name buffer)) . ,buffer))
				(emms-metaplaylist-mode-sorted-buffer-list))))
	 (consult--read playlist-list
			:prompt "EMMS Playlist: "
			:require-match t
			:default (buffer-name emms-playlist-buffer)
			:lookup #'consult--lookup-cdr
			:sort nil)))

;;;; Entry Points

;;;###autoload
(defun consult-emms-library ()
  (interactive)
  (consult--multi consult-emms-library-sources
                 :require-match t
                 :prompt "EMMS Library: "))

(defvar consult-emms--playlists-history nil
  "History of `consult-emms-playlists'.")

;;;###autoload
(defun consult-emms-playlists ()
  "Select a track from an EMMS buffer. Each buffer is a category."
  (interactive)
  (let ((playlists (mapcar 'consult-emms--playlist-source-from-buffer
			 (emms-metaplaylist-mode-sorted-buffer-list))))
    (consult--multi playlists
		    :require-match t
		    :prompt "Track: "
		    :history 'consult-emms--playlists-history)))

;;;###autoload
(defun consult-emms-metaplaylist ()
  "Choose an EMMS playlist to interact with."
  (interactive)
  (let ((buffer (consult-emms--choose-buffer)))
    (consult-emms--playlist buffer)))

;;;###autoload
(defun consult-emms-current-playlist ()
  "Select a track from the current EMMS playlist."
  (interactive)
  (consult-emms--playlist emms-playlist-buffer))

(provide 'consult-emms)

;;; consult-emms.el ends here

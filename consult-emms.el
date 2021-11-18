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

(defcustom consult-emms-library-sources '(consult-emms--source-track
					  consult-emms--source-album)
  "Sources used by `consult-emms-library'.

See `consult--multi' for a description of the source values. The
only exception is :cache. If non-nil, the source's will use cache
values between uses. This is useful for sources which need to do
complex operations on many candidates (like for albums)."
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

(defun consult-emms--compare-track-numbers (a b)
  "Return t if track with key A has a lower tracknumber than B."
  (< (string-to-number (assoc-default 'info-tracknumber (gethash a emms-cache-db) nil nil))
     (string-to-number (assoc-default 'info-tracknumber (gethash b emms-cache-db) nil nil))))

(defun consult-emms--add-album (album)
  (mapcar (lambda (trk)
	    (emms-add-file (assoc-default 'name (gethash trk emms-cache-db) nil nil)))
	  (sort (gethash album consult-emms--album-cache)
		'consult-emms--compare-track-numbers)))

(defvar consult-emms--album-history nil
  "History of `consult-emms--source-album'.")

(defvar consult-emms--source-album
  '(:name     "Album"
    :narrow   ?b
    :category album
    :history  consult-emms--album-history
    :items    consult-emms--get-albums
    :action   consult-emms--add-album
    :cache t)
  "Album source for `consult-emms-library'.")

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

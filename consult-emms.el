;;; consult-emms.el --- Consult interface to EMMS

;; Author: Hugo Heagren <hugo@heagren.com>
;; Version: 0.1
;; Package-Requires: ((consult) (emms))
;; Keywords: consult, emms

;;; Code:

(require 'consult)
(require 'emms)

;;;###autoload
(defun consult-emms-library ()
  (interactive)
  (consult--multi consult-emms-library-sources
                 :require-match t
                 :prompt "EMMS Library: "
                 :history 'consult--buffer-history))

(provide 'consult-emms)

;;; consult-emms.el ends here

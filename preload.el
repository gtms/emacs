;;; preload.el --- personal modifications to take effect prior to Emacs Prelude
;;; loading

;;; Commentary:
;; This program contains personal modifications to Emacs Prelude
;; To be symlinked into ~/.emacs.d/personal/

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section: colour themes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; zenburn if on OS X; solarized-light if on gnu/linux

(cond
 ((string-equal system-type "gnu/linux")
  (setq prelude-theme 'base16-eighties-dark)
  ;; (setq prelude-theme 'base16-atelierseaside-dark)
  )
 )

;;; preload.el ends here

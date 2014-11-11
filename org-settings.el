;;; org-settings.el --- personal modifications to Emacs Org Mode

;;; Commentary:
;; This program contains personal modifications to Emacs Org Mode
;; From the Org Mode Info documentation
;; To be symlinked into ~/.emacs.d/personal/

;;; Code:
;; sets org default directory
(setq org-directory "~/Dropbox/org/")

;; from 9.1.1 Setting up capture
(setq org-default-notes-file (concat org-directory "notes/notes.org"))
(define-key global-map "\C-cc" 'org-capture)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-settings.el ends here

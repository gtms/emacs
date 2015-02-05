;;; org-settings.el --- personal modifications to Emacs Org Mode

;;; Commentary:
;; This program contains personal modifications to Emacs Org Mode
;; From the Org Mode Info documentation
;; To be symlinked into ~/.emacs.d/personal/

;;; Code:
;; sets org default directory
(setq org-directory "~/Dropbox/org/")

;; sets variable org-agenda-files
(setf org-agenda-files (concat org-directory "todo/agenda-files"))

;; from 9.1.1 Setting up capture
(setq org-default-notes-file (concat org-directory "todo/todo.org"))
(define-key global-map "\C-cc" 'org-capture)

;; from 5.2.1 TODO keywords as workflow states
(setq org-todo-keywords
      '((sequence "TODO(t)" "STARTED(s)" "WAITING(w)" "APPT(a)" "|" "DONE(d)" "CANCELED(c)")))

;; (setq org-refile-use-outline-path 'file)
(setq org-refile-targets '((nil :maxlevel . 3)
                                        ; all top-level headlines in the
                                        ; current buffer are used (first) as a
                                        ; refile target
                           (org-agenda-files :maxlevel . 3)))

(setq org-reverse-note-order t)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;; org-settings.el ends here

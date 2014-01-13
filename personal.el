;; dired sorts directories first
;; from http://www.emacswiki.org/emacs/DiredSortDirectoriesFirst
;; 03Oct2012
(defun mydired-sort ()
  "Sort dired listings with directories first."
  (save-excursion
    (let (buffer-read-only)
      (forward-line 2) ;; beyond dir. header
      (sort-regexp-fields t "^.*$" "[ ]*." (point) (point-max)))
    (set-buffer-modified-p nil)))

(defadvice dired-readin
  (after dired-after-updating-hook first () activate)
  "Sort dired listings with directories first before adding marks."
  (mydired-sort))
(put 'dired-find-alternate-file 'disabled nil)

;; cancels prelude-ui.el instruction to render the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 8))

;; toggles delete-selection-mode on
(delete-selection-mode 1)

;; toggles display-time mode on
(setq display-time-day-and-date t)
(display-time)

;; toggles electric-pair-mode on
;; (electric-pair-mode 1)

;; enable skeleton-pair insert globally
(setq skeleton-pair t)
;;(setq skeleton-pair-on-word t)
(global-set-key (kbd "(") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "[") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "{") 'skeleton-pair-insert-maybe)
(global-set-key (kbd "\"") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "\'") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "\`") 'skeleton-pair-insert-maybe)
;; (global-set-key (kbd "<") 'skeleton-pair-insert-maybe)

;; sets alternate regexp heading in outline mode to '## *+'
(setq outline-regexp " *## \\(*\\)+")

;; initializes ess and adds hook for orgstruct-mode
;; 07Jan2014
(require 'ess-site)
(add-hook 'ess-mode-hook 'turn-on-orgstruct++)

;; miscellaneous configurations
;; 13Jan2014
;;
;; removes all scroll bars
(scroll-bar-mode -1)
;; activates blink-cursor-mode
(blink-cursor-mode 1)
;; toggles visualization of matching parenthesis
(show-paren-mode 1)
;; configures emacs so that word moving commands will move cursor into
;; between CamelCaseWords
(global-subword-mode 1)
;; sets set-mark-command-repeat-pop to TRUE
;; non-nil means repeating C-SPC after popping mark pops it again
(setq set-mark-command-repeat-pop t)
;; defines variable dired-listing-switches
(setq dired-listing-switches "-alh")
;; defines variable orgstruct-heading-prefix-regexp
(setq orgstruct-heading-prefix-regexp "## ")

;; disables proced ("C-x p" is too often accidentally typed)
;; 13Jan2014
(put 'proced 'disabled t)

;;; personal.el ends here

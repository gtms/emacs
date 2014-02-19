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

;; sets alternate regexp heading in outline mode to '## *+'
;; (setq outline-regexp " *## \\(*\\)+")

;; initializes ess and adds hook for orgstruct-mode
;; 07Jan2014
(require 'ess-site)
(add-hook 'ess-mode-hook 'turn-on-orgstruct)
;; adds hook for smartparens
;; 19Feb2014
(add-hook 'ess-mode-hook (lambda () (smartparens-mode 1)))
(add-hook 'ess-post-run-hook 'smartparens-mode)

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

;; sets default theme to solarized-dark
;; according to instructions from solarized-dark-theme.el
;; 16Jan2014
;; (require 'solarized)
;; (deftheme solarized-dark "The dark variant of the Solarized colour theme")
;; (create-solarized-theme 'dark 'solarized-dark)
;; (provide-theme 'solarized-dark)
;; 30Jan2014
;; (disable-theme 'zenburn)
;; (load-theme 'solarized-dark t)

;; cancels key-chords defined by prelude-key-chord.el
;; 27Jan2014
;; undo-tree-visualize
(key-chord-define-global "uu" nil)
;; execute-extended-command
(key-chord-define-global "xx" nil)
;; browse-kill-ring
(key-chord-define-global "yy" nil)

;; replace-colorthemes
;; https://github.com/emacs-jp/replace-colorthemes
;; 31Jan2014
;; Please set your themes directory to 'custom-theme-load-path
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/themes/replace-colorthemes"))
;; load your favorite theme
;; (load-theme 'dark-laptop t t)
;; (enable-theme dark-laptop)

;;; personal.el ends here

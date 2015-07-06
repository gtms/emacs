;;; personal.el --- personal modifications to Emacs Prelude

;;; Commentary:
;; This program contains personal modifications to Emacs Prelude
;; To be symlinked into ~/.emacs.d/personal/

;;; Code:

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section: prelude stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subsection:prelude-require
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(prelude-require-packages '(ido-vertical-mode
                            visual-regexp
                            relative-line-numbers
                            ibuffer-vc
                            hungry-delete
                            exec-path-from-shell
                            zotelo
                            smart-mode-line
                            ;; smart-mode-line-powerline-theme
                            powerline
                            ess
                            ess-R-data-view
                            2048-game))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section: my keybindings
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Bind C-o to ace-window
;; 02Oct2014
(global-set-key (kbd "C-x o") 'ace-window)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section: package configuration
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Adds ESS-specific smartparens hooks
;;
;; adds hook for smartparens
;; 19Feb2014
(add-hook 'ess-mode-hook (lambda () (smartparens-mode 1)))
(add-hook 'ess-post-run-hook 'smartparens-mode)
;; Solves M-r keybinding conflict:
;; calls for comint-history-isearch-backward-regexp in ess
;; and for sp-splice-sexp-killing-around in smartparens
;; Here we choose to keep the ess functionality
;; 21Feb2014
(add-hook 'smartparens-mode-hook
          (lambda ()
            (define-key smartparens-mode-map [?\M-r] nil)))

;; Configures ido-vertical-mode
(require 'ido-vertical-mode)
(ido-mode 1)
(ido-vertical-mode 1)
(setq ido-vertical-define-keys 'C-n-and-C-p-only)

;; Configures smart-mode-line
;; 10Sep2014
(require 'smart-mode-line)
(sml/setup)
;; (sml/apply-theme 'respectful)
;; (sml/apply-theme 'powerline)

;; configures exec-path-from-shell
;; 08Sep2014
(exec-path-from-shell-initialize)

;; Installs csv-mode as per instructions found on csv-mode.el
;; 20Feb2014
(add-to-list 'auto-mode-alist '("\\.[Cc][Ss][Vv]\\'" . csv-mode))
(autoload 'csv-mode "csv-mode"
  "Major mode for editing comma-separated value files." t)

;; my org-mode stuff
;; 25Feb2014
(add-hook 'org-mode-hook 'org-indent-mode)

;; Configures RefteX
;; 05Feb2015
(add-hook 'LaTeX-mode-hook 'turn-on-reftex)   ; with AUCTeX LaTeX mode
(add-hook 'latex-mode-hook 'turn-on-reftex)   ; with Emacs latex mode

;; Configures zotelo
;; 21Feb2015
;; from zotelo.el
(require 'zotelo)
(add-hook 'TeX-mode-hook 'zotelo-minor-mode)

;; Activates rich-minority-mode
;; 06Jul2015
(rich-minority-mode 1)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subsection: reconfigure prelude
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; cancels prelude-ui.el instruction to render the fringe (gutter) smaller
;; the argument is a width in pixels (the default is 8)
(if (fboundp 'fringe-mode)
    (fringe-mode 8))

;; disables proced ("C-x p" is too often accidentally typed)
;; 13Jan2014
;; proced was replaced by vkill on prelude: disable vkill instead
;; 29Jul2014
(put 'vkill 'disabled t)

;; cancels key-chords defined by prelude-key-chord.el
;; 27Jan2014
;; undo-tree-visualize
;; (key-chord-define-global "uu" nil)
;; execute-extended-command
;; (key-chord-define-global "xx" nil)
;; browse-kill-ring
;; (key-chord-define-global "yy" nil)



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; section: personal hacks
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subsection: miscellaneous configurations
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; toggles delete-selection-mode on
(delete-selection-mode 1)
;; toggles display-time mode on
(setq display-time-day-and-date t)
(display-time)
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
(defvar orgstruct-heading-prefix-regexp)
(setq orgstruct-heading-prefix-regexp "## ")
;; configures global hungry-delete-mode
;; 01Aug2014
(global-hungry-delete-mode)
;; uncomment this to use default theme
;; (disable-theme 'zenburn)
;; toggle letter case
;; (defun toggle-letter-case ()
;;   "Toggle the letter case of current word or text selection.
;; Toggles between: “all lower”, “Init Caps”, “ALL CAPS”."
;;   (interactive)
;;   (let (p1 p2 (deactivate-mark nil) (case-fold-search nil))
;;     (if (region-active-p)
;;         (setq p1 (region-beginning) p2 (region-end))
;;       (let ((bds (bounds-of-thing-at-point 'word) ) )
;;         (setq p1 (car bds) p2 (cdr bds)) ) )

;;     (when (not (eq last-command this-command))
;;       (save-excursion
;;         (goto-char p1)
;;         (cond
;;          ((looking-at "[[:lower:]][[:lower:]]") (put this-command 'state "all lower"))
;;          ((looking-at "[[:upper:]][[:upper:]]") (put this-command 'state "all caps") )
;;          ((looking-at "[[:upper:]][[:lower:]]") (put this-command 'state "init caps") )
;;          ((looking-at "[[:lower:]]") (put this-command 'state "all lower"))
;;          ((looking-at "[[:upper:]]") (put this-command 'state "all caps") )
;;          (t (put this-command 'state "all lower") ) ) )
;;       )

;;     (cond
;;      ((string= "all lower" (get this-command 'state))
;;       (upcase-initials-region p1 p2) (put this-command 'state "init caps"))
;;      ((string= "init caps" (get this-command 'state))
;;       (upcase-region p1 p2) (put this-command 'state "all caps"))
;;      ((string= "all caps" (get this-command 'state))
;;       (downcase-region p1 p2) (put this-command 'state "all lower")) )
;;     )
;;   )
;;set this to M-c
;; (global-set-key "\M-c" 'toggle-letter-case)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subsection: former modifications
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Configuring EMACS: making TRAMP use ssh-agent
;; 22May2014
;; http://dietbuddha.blogspot.be/2012/04/configuring-emacs-making-tramp-use-ssh.html
;; (setenv "SSH_AUTH_SOCK" (concat (getenv "HOME") "/.ssh-auth-sock"))

;; some emacs-lisp-mode stuff
;; 25Feb2014
;; from https://github.com/ankurdave/dotfiles/blob/master/.emacs.d/hooks.el
;; (add-hook 'emacs-lisp-mode-hook 'eldoc-mode)
;; (when (fboundp 'smartparens-mode)
;;   (add-hook 'emacs-lisp-mode-hook 'smartparens-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subsection: TRAMP
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; sets TRAMP to use aliases in ~/.ssh/config
(tramp-set-completion-function "ssh"
                               '((tramp-parse-sconfig "/etc/ssh_config")
                                 (tramp-parse-sconfig "~/.ssh/config")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subsection: dired stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
  "Sort dired listings with directories first before adding mark."
  (mydired-sort))
(put 'dired-find-alternate-file 'disabled nil)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subsection: ESS stuff
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; initializes ess and adds hook for orgstruct-mode
;; 07Jan2014
(require 'ess-site)
(add-hook 'ess-mode-hook 'turn-on-orgstruct)
;; adds hook for smartparens
;; 19Feb2014
;; (add-hook 'ess-mode-hook (lambda () (smartparens-mode 1)))
;; (add-hook 'ess-post-run-hook 'smartparens-mode)
;; Solves M-r keybinding conflict:
;; calls for comint-history-isearch-backward-regexp in ess
;; and for sp-splice-sexp-killing-around in smartparens
;; Here we choose to keep the ess functionality
;; 21Feb2014
(add-hook 'smartparens-mode-hook
          (lambda ()
            (define-key smartparens-mode-map [?\M-r] nil)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subsection: replace-colorthemes
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; https://github.com/emacs-jp/replace-colorthemes
;; 31Jan2014
;; Please set your themes directory to 'custom-theme-load-path
(add-to-list 'custom-theme-load-path
             (file-name-as-directory "~/.emacs.d/themes/replace-colorthemes"))
;; load your favorite theme
;; (load-theme 'dark-laptop t t)
;; (enable-theme dark-laptop)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; subsection: ibuffer related content
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; see http://www.emacswiki.org/emacs/IbufferMode
;; 30Jul2014
;; Use human readable Size column instead of original one
(define-ibuffer-column size-h
  (:name "Size" :inline t)
  (cond
   ((> (buffer-size) 1000000) (format "%7.1fM" (/ (buffer-size) 1000000.0)))
   ((> (buffer-size) 100000) (format "%7.0fk" (/ (buffer-size) 1000.0)))
   ((> (buffer-size) 1000) (format "%7.1fk" (/ (buffer-size) 1000.0)))
   (t (format "%8d" (buffer-size)))))

;; Modify the default ibuffer-formats
(setq ibuffer-formats
      '((mark modified read-only " "
              (name 18 18 :left :elide)
              " "
              (size-h 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " "
              filename-and-process)))

;; From ibuffer-vc.el
(add-hook 'ibuffer-hook
          (lambda ()
            (ibuffer-vc-set-filter-groups-by-vc-root)
            (unless (eq ibuffer-sorting-mode 'alphabetic)
              (ibuffer-do-sort-by-alphabetic))))

;; turn off ibuffer-show-empty-filter-groups
;; From http://martinowen.net/blog/2010/02/03/tips-for-emacs-ibuffer.html
(setq ibuffer-show-empty-filter-groups nil)

;;; personal.el ends here

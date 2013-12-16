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
(display-time-mode 1)

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

;;; personal.el ends here

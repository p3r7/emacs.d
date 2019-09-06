
;; ------------------------------------------------------------------------
;; BOOKMARK

(use-package bookmark
  :ensure nil
  :bind (("<f10>" . list-bookmarks)
         ("C-<f10>" . bookmark-set)
         :map bookmark-bmenu-mode-map
         ("s" . prf/bmkp-bmenu-open-shell))
  :init
  (setq
   bookmark-default-file "~/.emacs.d/bookmarks" ;; keep my ~/ clean
   bookmark-save-flag 1                         ;; autosave each change
   Buffer-menu-name-width 40)
  :config
  (add-to-list 'display-buffer-alist `(,(concat "^\\*Bookmark List\\*$") display-buffer-same-window)))


;; ------------------------------------------------------------------------
;; BOOKMARK+

(if (windows-nt-p)
    (use-package bookmark+
      :load-path "~/.emacs.d/plugins/bookmark+"
      :after (bookmark))
  (use-package bookmark+
    :quelpa (bookmark+ :fetcher github :repo "emacsmirror/bookmark-plus")
    :after (bookmark)))


;; ------------------------------------------------------------------------
;; CUSTOM ACTIONS

;; copy of bookmark-bmenu-this-window
(defun prf/bmkp-bmenu-action (&optional flip-use-region-p action-fucntion) ; Bound to `RET' in bookmark list
  "Select this line's bookmark in this window.
See `bookmark-jump' for info about the prefix arg.
Function inspired by `bookmark-bmenu-this-window' from bookmark+."
  (bmkp-bmenu-barf-if-not-in-menu-list)
  (bookmark-bmenu-ensure-position)
  (let ((bookmark-name  (bookmark-bmenu-bookmark)))
    (action-fucntion bookmark-name 'switch-to-buffer flip-use-region-p)))

(defun prf/bmkp-bmenu-open-shell (&optional flip-use-region-p action-fucntion)
  (interactive "P")
  (prf/bmkp-bmenu-action flip-use-region-p action-fucntion))

(defun prf/bmkp-jump-shell (bookmark display-function &optional flip-use-region-p)
  "Helper function for `prf/bmkp-bmenu-action' commands.
Function inspired by `bmkp-jump-1' from bookmark+."
  (setq bookmark  (bookmark-get-bookmark bookmark 'NOERROR))
  (unless bookmark (error "No bookmark specified"))
  (run-hooks 'bmkp-before-jump-hook)
  (bookmark-maybe-historicize-string (bmkp-bookmark-name-from-record bookmark))
  (let ((bmkp-use-region  (if flip-use-region-p (not bmkp-use-region) bmkp-use-region)))
    (setq bookmark (bookmark-get-filename bookmark))
    (when bookmark
      (cd (expand-file-name bookmark))
      (call-interactively #'prf/tramp/shell))))



(provide 'init-bookmark+)



;; BOOKMARK

(use-package bookmark
  :ensure nil
  :bind (("<f10>" . list-bookmarks)
         ("C-<f10>" . bookmark-set)
         :map bookmark-bmenu-mode-map
         ("s" . prf/bmkp-bmenu-open-shell)
         ("M-!" . prf/bmkp-bmenu-shell-command)
         ("M-&" . prf/bmkp-bmenu-async-shell-command)
         ("M-:" . prf/bmkp-bmenu-eval-expression)
         ("C-x f" . prf/bmkp-bmenu-lusty-explorer)
         ("C-x C-f" . prf/bmkp-bmenu-lusty-explorer)
         ("C-c M-x" . prf/bmkp-bmenu-M-x))
  :init
  (setq
   bookmark-default-file "~/.emacs.d/bookmarks" ;; keep my ~/ clean
   bookmark-save-flag 1                         ;; autosave each change
   Buffer-menu-name-width 40)
  :config
  (add-to-list 'display-buffer-alist `(,(concat "^\\*Bookmark List\\*$") display-buffer-same-window)))



;; BOOKMARK+

(use-package bookmark+
  :quelpa (bookmark+ :fetcher github :repo "emacsmirror/bookmark-plus")
  :after bookmark
  :hook ((bookmark-bmenu-mode
          . (lambda ()
              ;; NB: recent bookmark+ introduced a key prefix "s" to sort bookmarks
              ;; we need a hook to override it
              (bind-key "s" #'prf/bmkp-bmenu-open-shell bookmark-bmenu-mode-map)
              ))))



;; CUSTOM ACTIONS

(defun prf/bmkp-bmenu-action (action-function &optional flip-use-region-p) ; Bound to `RET' in bookmark list
  "Select this line's bookmark in this window.
See `bookmark-jump' for info about the prefix arg.
Function inspired by `bookmark-bmenu-this-window' from bookmark+."
  (bmkp-bmenu-barf-if-not-in-menu-list)
  (bookmark-bmenu-ensure-position)
  (let ((bookmark-name (bookmark-bmenu-bookmark)))
    (prf/bmkp-jump-action bookmark-name action-function 'switch-to-buffer flip-use-region-p)))

(defun prf/bmkp-jump-action (bookmark action-function display-function &optional flip-use-region-p)
  "Helper function for `prf/bmkp-bmenu-action' commands.
Function inspired by `bmkp-jump-1' from bookmark+."
  (setq bookmark (bookmark-get-bookmark bookmark 'NOERROR))
  (unless bookmark (error "No bookmark specified"))
  (run-hooks 'bmkp-before-jump-hook)
  (bookmark-maybe-historicize-string (bmkp-bookmark-name-from-record bookmark))
  (let ((bmkp-use-region  (if flip-use-region-p (not bmkp-use-region) bmkp-use-region)))
    (setq bookmark (bookmark-get-filename bookmark))
    (when bookmark
      (let ((dir-location (expand-file-name bookmark)))
        (unless (file-directory-p dir-location)
          (setq dir-location (file-name-directory dir-location)))
        (cd dir-location)
        (call-interactively action-function)))))

(defun prf/bmkp-bmenu-open-shell (&optional flip-use-region-p)
  (interactive "P")
  (prf/bmkp-bmenu-action #'friendly-shell flip-use-region-p))

(defun prf/bmkp-bmenu-shell-command (&optional flip-use-region-p)
  (interactive "P")
  (prf/bmkp-bmenu-action #'shell-command flip-use-region-p))

(defun prf/bmkp-bmenu-async-shell-command (&optional flip-use-region-p)
  (interactive "P")
  (prf/bmkp-bmenu-action #'async-shell-command flip-use-region-p))

(defun prf/bmkp-bmenu-eval-expression (&optional flip-use-region-p)
  (interactive "P")
  (prf/bmkp-bmenu-action #'eval-expression flip-use-region-p))

(defun prf/bmkp-bmenu-M-x (&optional flip-use-region-p)
  (interactive "P")
  (prf/bmkp-bmenu-action #'helm-M-x flip-use-region-p))

(defun prf/bmkp-bmenu-lusty-explorer (&optional flip-use-region-p)
  (interactive "P")
  (prf/bmkp-bmenu-action #'lusty-file-explorer flip-use-region-p))




(provide 'init-bookmark+)

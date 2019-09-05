
;; ------------------------------------------------------------------------
;; BOOKMARK

(use-package bookmark
  :ensure nil
  ;; :demand
  :bind (("<f10>" . list-bookmarks)
         ("C-<f10>" . bookmark-set))
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
(defun prf/bookmark-bmenu-shell-this-window (&optional flip-use-region-p) ; Bound to `RET' in bookmark list
  "Select this line's bookmark in this window.
See `bookmark-jump' for info about the prefix arg."
  (interactive "P")
  (bmkp-bmenu-barf-if-not-in-menu-list)
  (bookmark-bmenu-ensure-position)
  (let ((bookmark-name  (bookmark-bmenu-bookmark)))
    (bmkp-jump-1 bookmark-name 'switch-to-buffer flip-use-region-p)))

(defun prf/bmkp-jump-shell (bookmark display-function &optional flip-use-region-p)
  "Helper function for `bookmark-jump' commands.
BOOKMARK is a bookmark name or a bookmark record.
DISPLAY-FUNCTION is passed to `bookmark--jump-via'.
Non-nil optional arg FLIP-USE-REGION-P means temporarily flip the
 value of `bmkp-use-region'."
  (setq bookmark  (bookmark-get-bookmark bookmark 'NOERROR))
  (unless bookmark (error "No bookmark specified"))
  (run-hooks 'bmkp-before-jump-hook)
  (bookmark-maybe-historicize-string (bmkp-bookmark-name-from-record bookmark))
  (let ((bmkp-use-region  (if flip-use-region-p (not bmkp-use-region) bmkp-use-region)))
    (bookmark--jump-via bookmark display-function)))



(provide 'init-bookmark+)

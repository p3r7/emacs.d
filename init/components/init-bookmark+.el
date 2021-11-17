

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
              )))

  :config
  ;; NB: own version that reconstruct full tramp path, including multi-hop
  (defun bookmark-buffer-file-name ()
    "Return the current buffer's file in a way useful for bookmarks."
    ;; Abbreviate the path, both so it's shorter and so it's more
    ;; portable.  E.g., the user's home dir might be a different
    ;; path on different machines, but "~/" will still reach
    (let ((fn (cond
               (buffer-file-name buffer-file-name)
               ((and (boundp 'dired-directory) dired-directory)
                (if (stringp dired-directory)
                    dired-directory
                  (car dired-directory)))
               (t (error "Buffer not visiting a file or directory")))))
      (if (ignore-errors (tramp-dissect-file-name fn))
          (prf/tramp/complete-file-name fn)
        (abbreviate-file-name fn))))

  (defun bmkp-make-dired-record ()
    "Create and return a Dired bookmark record."
    (let ((hidden-dirs  (save-excursion (dired-remember-hidden))))
      (unwind-protect
          (let* ((dir      (expand-file-name (if (consp dired-directory)
                                                 (file-name-directory (car dired-directory))
                                               dired-directory)))
                 (real-dir (if (ignore-errors (tramp-dissect-file-name dir))
                               (prf/tramp/complete-file-name dir)
                             dir))
                 (subdirs  (bmkp-dired-subdirs))
                 (mfiles   (bmkp-dired-remember-*-marks (point-min) (point-max))))
            `(,dir
              ,@(bookmark-make-record-default 'NO-FILE)
              ;; (filename . ,real-dir) (dired-directory . ,dired-directory)
              (filename . ,real-dir) (dired-directory . ,real-dir)
              (dired-marked . ,mfiles) (dired-switches . ,dired-actual-switches)
              (dired-subdirs . ,subdirs) (dired-hidden-dirs . ,hidden-dirs)
              (handler . bmkp-jump-dired)))
        (save-excursion                   ; Hide subdirs that were hidden.
          (dolist (dir  hidden-dirs)  (when (dired-goto-subdir dir) (dired-hide-subdir 1))))))))



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

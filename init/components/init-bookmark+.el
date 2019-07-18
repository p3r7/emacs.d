
(setq Buffer-menu-name-width 40)

(if (windows-nt-p)
    (when (prf/require-plugin 'bookmark+ nil 'noerror)
      (global-set-key [f10] 'list-bookmarks)
      (global-set-key (kbd "C-<f10>") 'bookmark-set)
      (add-to-list 'display-buffer-alist `(,(concat "^\\*Bookmark List\\*$") display-buffer-same-window))
      (setq
       bookmark-default-file "~/.emacs.d/bookmarks" ;; keep my ~/ clean
       bookmark-save-flag 1))                        ;; autosave each change
  (use-package bookmark+
    :quelpa (bookmark+ :fetcher github :repo "emacsmirror/bookmark-plus")
    :bind (("<f10>" . list-bookmarks)
	   ("C-<f10>" . bookmark-set))
    :init
    (setq
     bookmark-default-file "~/.emacs.d/bookmarks" ;; keep my ~/ clean
     bookmark-save-flag 1)                        ;; autosave each change
    :config
    (add-to-list 'display-buffer-alist `(,(concat "^\\*Bookmark List\\*$") display-buffer-same-window))))


;; (use-package bookmark+
;;   ;; :load-path "~/.emacs.d/plugins/bookmark+"
;;   :quelpa (bookmark+ :fetcher github :repo "emacsmirror/bookmark-plus"))
;;   :bind (("<f10>" . list-bookmarks)
;; 	 ("C-<f10>" . bookmark-set))
;;   :init
;;   (setq
;;    bookmark-default-file "~/.emacs.d/bookmarks" ;; keep my ~/ clean
;;    bookmark-save-flag 1)                        ;; autosave each change
;;   (add-to-list 'display-buffer-alist `(,(concat "^\\*Bookmark List\\*$") display-buffer-same-window)))

(use-package list-register
  :bind ("C-x r l" . list-register))

(provide 'init-bookmark+)

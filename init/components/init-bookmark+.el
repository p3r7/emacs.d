
(setq Buffer-menu-name-width 40)

(use-package bookmark+
  :load-path "~/.emacs.d/plugins/bookmark+"
  :bind (("<f10>" . list-bookmarks)
	 ("C-<f10>" . bookmark-set))
  :init
  (setq
   bookmark-default-file "~/.emacs.d/bookmarks" ;; keep my ~/ clean
   bookmark-save-flag 1)                        ;; autosave each change
  ;; don't create new buffer
  (add-to-list 'display-buffer-alist `(,(concat "^\\*Bookmark List\\*$") display-buffer-same-window)))

(use-package list-register
  :bind ("C-x r l" . list-register))

(provide 'init-bookmark+)

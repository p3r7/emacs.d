
(setq Buffer-menu-name-width 40)

(when (prf/require-plugin 'bookmark+ nil 'noerror)
  (global-set-key [f10] 'list-bookmarks)
  (global-set-key (kbd "C-<f10>") 'bookmark-set)
  ;; TODO: optimize this
  (add-to-list 'display-buffer-alist `(,(concat "^\\*Bookmark List\\*$") display-buffer-same-window))
  (setq
   bookmark-default-file "~/.emacs.d/bookmarks" ;; keep my ~/ clean
   bookmark-save-flag 1)                        ;; autosave each change
  )


(when (require 'list-register nil 'noerror)
  (global-set-key (kbd "C-x r l") 'list-register))


(provide 'init-bookmark+)

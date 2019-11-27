
(use-package yasnippet
  :demand
  :delight yas-minor-mode

  :bind (
         :map yas-minor-mode-map
         ("C-c y" . yas-expand))

  :init
  (setq yas-alias-to-yas/prefix-p nil)

  :config
  ;; originally bounded to yas-maybe-expand, conflict w/ ac
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  ;; NB: this prevents nested expansions

  (defun prf/yas/just-expanded-p ()
    ;; TODO: test also finished expansion that just ended
    (yas-active-snippets))

  (defun prf/yas-maybe-expand-or-cancel-key-filter (cmd)
    (if (prf/yas/just-expanded-p)
        cmd
      (yas-maybe-expand-abbrev-key-filter cmd)))

  (defun prf/yas-expand-or-cancel ()
    (interactive)
    (if (yas-active-snippets)
        (progn
          (yas-abort-snippet (car (yas-active-snippets)))
          (undo)
          (just-one-space)))
    (yas-expand))

  (defconst prf/yas-maybe-expand-or-cancel
    '(menu-item "" prf/yas-expand-or-cancel :filter prf/yas-maybe-expand-or-cancel-key-filter)
    "A conditional key definition.
This can be used as a key definition in keymaps to bind a key to
both `yas-expand' (when there is a snippet available to be expanded)
and `yas-abort-snippet' (already during expansion).
Inspired by `yas-maybe-expand'")

  ;; (define-key yas-minor-mode-map (kbd "SPC") prf/yas-maybe-expand-or-cancel)

  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after (yasnippet))

(provide 'init-yasnippet)

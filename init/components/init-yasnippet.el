
(use-package yasnippet
  :demand
  :delight yas-minor-mode

  :bind (
         :map yas-minor-mode-map
         ("C-c y" . yas-expand))

  :init
  (setq yas-alias-to-yas/prefix-p nil)
  (setq yas-buffer-local-condition
        yas-not-string-or-comment-condition)

  :config
  ;; originally bounded to yas-maybe-expand, conflict w/ ac
  (define-key yas-minor-mode-map (kbd "<tab>") nil)
  (define-key yas-minor-mode-map (kbd "TAB") nil)

  ;; TODO: add blacklist / whitelist in yas--templates-for-key-at-point
  ;; just has to advice yas--templates-for-key-at-point for filter the car of return value (templates-and-pos)
  ;; even better: store last applied in a var and expose a command to blacklist last played

  (defvar prf/yas-template-blacklist nil "Blacklist of templates to not play")
  (defvar prf/last-templates-and-pos nil "For debugging purposes")

  ;; BUG: doesn't get triggered, env though not autoloaded...
  (defadvice prf/yas-filter-templates (around yas--templates-for-key-at-point activate)

    (message "Triggered")

    (let ((templates-and-pos ad-do-it))
      (when templates-and-pos

        (setq prf/last-templates-and-pos templates-and-pos)

        ;; REVIEW: there should be a better way to destructure
        (let ((templates (car templates-and-pos))
              (pos (cadr templates-and-pos))
              (original (cl-caddr templates-and-pos))))
        (setq templates
              (--filter (not (member (car it) prf/yas-template-blacklist)) templates))
        (when templates
          (list templates pos original)))))

  ;; TODO: for elisp, prevent expension:
  ;; - in quotted list (toggeable for edge-cases like eval-after-load)
  ;; - when at first element of a let expression
  ;; for this -> extend yas-buffer-local-condition

  ;; NB: this prevents nested expansions

  (defvar prf/yas-last-expand-success-timestamp nil)
  (defvar prf/yas-cancel-on-repeat-delay 1 "Max time to wait for allowing to cancel snippet expansion")

  (defun prf/yas-update-last-expand-success-timestamp ()
    (setq prf/yas-last-expand-success-timestamp (float-time)))

  (add-hook 'yas-before-expand-snippet-hook #'prf/yas-update-last-expand-success-timestamp)

  (defun prf/yas/just-expanded-p ()
    ;; TODO: test also finished expansion that just ended
    (or (yas-active-snippets)
        (>= prf/yas-cancel-on-repeat-delay
            (- (float-time) prf/yas-last-expand-success-timestamp))))

  (defun prf/yas-maybe-expand-or-cancel-key-filter (cmd)
    (if (prf/yas/just-expanded-p)
        cmd
      (yas-maybe-expand-abbrev-key-filter cmd)))

  (defun prf/yas-expand-or-cancel ()
    (interactive)
    (cond
     ((yas-active-snippets)
      (yas-abort-snippet (car (yas-active-snippets)))
      (undo 1)
      (just-one-space))
     ((>= prf/yas-cancel-on-repeat-delay
          (- (float-time) prf/yas-last-expand-success-timestamp))
      (undo 1)
      (just-one-space))
     (t
      (yas-expand))))

  (defconst prf/yas-maybe-expand-or-cancel
    '(menu-item "" prf/yas-expand-or-cancel :filter prf/yas-maybe-expand-or-cancel-key-filter)
    "A conditional key definition.
This can be used as a key definition in keymaps to bind a key to
both `yas-expand' (when there is a snippet available to be expanded)
and `yas-abort-snippet' (already during expansion).
Inspired by `yas-maybe-expand'")

  ;; (define-key yas-minor-mode-map (kbd "SPC") prf/yas-maybe-expand-or-cancel)
  ;; (define-key yas-minor-mode-map (kbd "SPC") nil)

  (yas-global-mode 1))

(use-package yasnippet-snippets
  :after (yasnippet))

(provide 'init-yasnippet)

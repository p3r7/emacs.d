

;; HELP / DESCRIBE (EMACS)

;; automatic display of local help
;; also used to display errors of eclim in mb
(use-package help-at-pt
  :ensure nil
  :demand

  :init
  (setq help-at-pt-display-when-idle t)
  (setq help-at-pt-timer-delay 0.1)

  :config
  (help-at-pt-set-timer))


(require 'init-eldoc)

;; NB: elisp / emacs -specific stuff in HELP section in ~/.emacs.d/init/lang/init-elisp.el



;; TEXINFO

;; TODO: http://ergoemacs.org/emacs/emacs_adding_browser_keys.html
(add-to-list 'Info-default-directory-list "~/.emacs.d/info")



;; EPUB

(require 'init-nov)



;; MAN

(defalias 'man 'woman)
(setq woman-use-own-frame nil)

(use-package tldr
  :init
  (setq tldr-enabled-categories '("common" "linux"))

  :config

  ;; patch to support multi lines
  (defun tldr--split-string-md (string separator)
    (let ((lines (split-string string separator)))
      (cl-loop
       with current-item = nil
       with inside-multiline = nil
       for line in lines

       if inside-multiline
       do (setq current-item (concat current-item separator line))
       else
       do
       (setq current-item line)

       if (s-starts-with? "`" (s-trim line))
       do (setq inside-multiline 't)

       if (and inside-multiline
               (s-ends-with? "`" (s-trim line)))
       do (setq inside-multiline nil)

       if (null inside-multiline)
       collect current-item)))

  (defun prf/tldr-render-markdown (command)
    (let* ((file-path (tldr-get-file-path-from-command-name command))
           (lines (tldr--split-string-md
                   (with-temp-buffer
                     (insert-file-contents file-path)
                     (buffer-string)) "\n")))
      (mapconcat (lambda (line)
                   (cond ((equal "" line)
                          "")
                         ((string-prefix-p "# " line)
                          (propertize (substring line 2) 'face 'tldr-title))
                         ((string-prefix-p "> " line)
                          (propertize (concat "    " (substring line 2)) 'face 'tldr-introduction))
                         ((string-prefix-p "- " line)
                          (concat "- "
                                  (propertize (substring line 2) 'face 'tldr-description)))
                         ((string-prefix-p "`" line)
                          ;; Strip leading/trailing back-ticks and add code block face
                          (setq line (propertize (substring line 1 -1) 'face 'tldr-code-block))
                          ;; Add command face
                          (setq line (replace-regexp-in-string
                                      (concat "^" command)
                                      (propertize command 'face 'tldr-command-itself)
                                      line 'fixedcase))
                          ;; Strip {{}} and add command argument face
                          (while (string-match "{{\\(.+?\\)}}" line)
                            (let ((argument (propertize (match-string 1 line)
                                                        'face 'tldr-command-argument)))
                              (setq line (replace-match argument 'fixedcase 'literal line 0))))
                          (concat "  " line))))
                 lines "\n")))
  (defalias #'tldr-render-markdown #'prf/tldr-render-markdown)

  (defun prf/helm-tldr ()
    "Helm interface for `tldr'."
    (interactive)
    (unless (require 'helm nil t)
      (user-error "Helm not available"))
    (require 'man)
    (let* ((default-cmd (Man-default-man-entry))
           (cmd (completing-read
                 (format "tldr%s"
                         (if (string= default-cmd "")
                             ": "
                           (format " (default %s): " default-cmd)))
                 (tldr-get-commands-list)
                 nil t nil nil default-cmd))
           (markdown (tldr-render-markdown cmd)))
      (helm :sources (helm-build-sync-source "tldr"
                       :header-name
                       (lambda (_)
                         (with-temp-buffer
                           (insert markdown)
                           (replace-regexp-in-string
                            "\n\n *" ": "
                            (buffer-substring-no-properties
                             (point-min)
                             (progn
                               (goto-char (point-min))
                               (forward-line 2)
                               (line-end-position))))))
                       :candidates
                       (lambda ()
                         (with-temp-buffer
                           (insert markdown)
                           (goto-char (point-min))
                           (let ((res) text code
                                 code-end-pos)
                             (while (re-search-forward "^- " nil 'no-error)
                               (setq text (buffer-substring (point) (line-end-position)))
                               (forward-line 2)
                               (back-to-indentation)
                               (setq code-end-pos
                                     (save-excursion
                                       (forward-sentence)
                                       (point)))
                               (setq code (buffer-substring (point) code-end-pos))
                               (push (concat text "\n" code) res))
                             (nreverse res))))
                       :multiline t
                       :coerce (lambda (candidate) (substring candidate (1+ (string-match "\n" candidate))))
                       :action
                       '(("Insert command" . insert)
                         ("Copy command to kill-ring" . kill-new)))
            :buffer "*helm tldr*")))
  (defalias #'helm-tldr #'prf/helm-tldr))

(use-package tldr-ext
  :load-path "~/.emacs.d/plugins/tldr-ext"
  :after tldr
  :config
  (when (and tldr-ext-directory-path
             (file-exists-p tldr-ext-directory-path))
    (tldr-ext-activate)))



;; WEBSITES / ARTICLES

(require 'init-pocket)




(provide 'init-doc)


;; http://trey-jackson.blogspot.fr/2010/10/emacs-tip-38-automatically-diff-binary.html


;; EDIFF

(use-package ediff
  :after (s)
  :init
  (setq ediff-split-window-function 'split-window-horizontally
	    ediff-window-setup-function 'ediff-setup-windows-plain
        ediff-auto-refine 'on
        ;; NB: best option is `--ignore-goedel' but only avail on modern versions
	    ;; ediff-diff-options "--text -w"
	    ediff-diff-options "-w" ;; ignore all white spaces
	    ;; ediff-diff-options "-Z -E -b" ;; ignore trailing white spaces + tab expansion
	    )
  :config

  (defun prf/ediff-adjust-options ()
    "Adjust `ediff-diff-options` according to buffer types."

    ;; remove "-w" opt (ignore whitespaces) for python
    (when (and (boundp 'ediff-buffer-A) (buffer-live-p ediff-buffer-A)
               (with-current-buffer ediff-buffer-A
                 (derived-mode-p 'python-mode 'python-ts-mode)))
      ;; (setq-local ediff-ignore-similar-regions nil)
      (setq-local ediff-diff-options "")

      ;; `ediff-set-actual-diff-options' is force-setting `ediff-actual-diff-options' to -w!!!
      (setq-local ediff-actual-diff-options "")
      ))

  (add-hook 'ediff-startup-hook #'prf/ediff-adjust-options)

  (defun ediff-toggle ()
    (interactive)
    (if (= (length (window-list)) 2)
	    (let ((left-buffer (buffer-file-name (window-buffer (car (window-list)))))
	          (right-buffer (buffer-file-name (window-buffer (car (cdr (window-list)))))))
	      (ediff left-buffer right-buffer))
      (message "invalid number of visible buffers (expected 2)"))))



;; ZTREE

(use-package ztree
  :init
  (setq ztree-diff-additional-options '("-w"))
  :config

  ;; less aggressive faces
  (face-spec-set
   'ztreep-diff-model-add-face
   '((t :inherit success))
   'face-defface-spec)
  (face-spec-set
   'ztreep-diff-model-diff-face
   '((t :inherit warning))
   'face-defface-spec)

  ;; allow diffing between remote hosts
  (defun prf/ztree-same-host-p (&optional _file1 _file2) t)
  (defalias 'ztree-same-host-p #'prf/ztree-same-host-p)

  ;; NB: default value is '("^\\.")
  (setq ztree-diff-filter-list '("\\.o$" "\\.bin$" "\\.elf$" "\\.hex$"))

  (defun ztree-toggle ()
    (interactive)

    (unless (= (length (window-list)) 2)
      (error "invalid number of visible buffers (expected 2)"))

    (let (dir-A dir-B)
      (if (and (buffer-file-name)
               (file-exists-p (buffer-file-name)))
          (setq dir-A buffer-file-name)
        (setq dir-A default-directory))

      (other-window 1)

      (if (and (buffer-file-name)
               (file-exists-p (buffer-file-name)))
          (setq dir-B buffer-file-name)
        (setq dir-B default-directory))

      (other-window 1)

      (unless (and (file-directory-p dir-A)
                   (file-directory-p dir-B))
        (error "not all buffers directories"))

      (delete-other-windows)
      (ztree-diff dir-A dir-B))))

;; NB: Old way to do this was w/ `ediff-trees', but `ztree' seems more
;; maintained.



(provide 'init-ediff)

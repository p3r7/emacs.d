
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

  (defun prf/buffer-python-p (buf)
    (and (buffer-live-p buf)
         (with-current-buffer buf
           (derived-mode-p 'python-mode 'python-ts-mode))))

  (defun prf/ediff-session-has-python-p ()
    (or (prf/buffer-python-p (bound-and-true-p ediff-buffer-A))
        (prf/buffer-python-p (bound-and-true-p ediff-buffer-B))
        (prf/buffer-python-p (bound-and-true-p ediff-buffer-C))))

  ;; NB: we disable the "-w" option for python buffers
  ;; (in fact any options)
  ;; we have to use an advice as `ediff' does weird shenanigans w/ using an internal `ediff-set-actual-diff-options' which can't be `setq-local' and that shadows `ediff-diff-options'
  (defun prf/ediff--around-make-diff-buffer (orig &rest args)
    (let* ((has-python (prf/ediff-session-has-python-p))
           (ediff-diff-options (if has-python "" ediff-diff-options))
           (ediff-actual-diff-options (if has-python "" ediff-actual-diff-options)))
      (apply orig args)))
  (advice-add 'ediff-make-diff2-buffer :around
              #'prf/ediff--around-make-diff-buffer)

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

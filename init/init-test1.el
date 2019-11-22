(require 'noflet)
(require 's)

(use-package multi-scratch
  :disabled
  :init
  (setq multi-scratch-buffer-name "scratch")
  :config
  (defalias '_msn 'multi-scratch-new))

(defun prf/thing-at-point (THING)
  "wrapper around thing-at-point to support other types"
  (cond
   ((string= THING 'url)
    (substring  (thing-at-point 'url) 7) )
   (t (thing-at-point THING))
   ) )

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  ;; TODO: use
  (noflet ((process-list ())) ad-do-it))


;; -------------------------------------------------------------------------
;; helper to move at line / col

(defun pos-at-line-col (l c)
  (goto-char (point-min))
  (forward-line (- l 1))
  (move-to-column c)
  (point))

(defun pos-at-line-col-joined (l-c)
  (let* ((l-c (s-split ":" l-c))
         (l (string-to-number (car l-c)))
         (c (string-to-number (car (cdr l-c)))))
    (pos-at-line-col l c)))


;; -------------------------------------------------------------------------
;; better moving around code blocks

(defun prf/smart-forward-list ()
  (interactive)
  (let ((matching (ignore-errors (scan-lists (point) 1 0))))
    (if matching
        (call-interactively #'forward-list)
      (call-interactively #'up-list))))

(defun prf/smart-backward-list ()
  (interactive)
  (let ((matching (ignore-errors (scan-lists (point) -1 0))))
    (if matching
        (call-interactively #'backward-list)
      (call-interactively #'backward-up-list))))

(global-set-key [remap forward-list] #'prf/smart-forward-list)
(global-set-key [remap backward-list] #'prf/smart-backward-list)


;; -------------------------------------------------------------------------

(defun perf/buffer-local-set-key (key command)
  (interactive "KSet key buffer-locally: \nCSet key %s buffer-locally to command: ")
  (let ((oldmap (current-local-map))
        (newmap (make-sparse-keymap)))
    (when oldmap
      (set-keymap-parent newmap oldmap))
    (define-key newmap key command)
    (use-local-map newmap)))


;; -------------------------------------------------------------------------
;; VAGRANT local / ssh toggle

;; either do a `vagrant global-status' or look at file ~/.vagrant.d/data/machine-index/index

(defun prf/tramp/vagrant/vagrant-server-p (&optional path)
  (setq path (if path path (buffer-file-name)))
  (let (vec method user host path)
    (setq vec (tramp-dissect-file-name path))
    (setq vec (prf/tramp/vec/with-new-localname vec "/vagrant"))
    (file-directory-p (prf/tramp/vec/undissect vec))))


(provide 'init-test1)

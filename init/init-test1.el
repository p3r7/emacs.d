;; (require 'noflet)
(require 'cl-lib)
(require 's)
(require 'tramp)



;; disable lock

;; NB: ever since mocing to emacs 28 pretest, regurlarly getting self-lock while on TRAMP
;; this is a drastic way to solve the issue

(setq create-lockfiles nil)



;; debug pauses

(use-package explain-pause-mode
  :disabled
  :straight (explain-pause-mode :type git :host github :repo "lastquestion/explain-pause-mode")
  :config
  (explain-pause-mode))



;; find-file

;; REVIEW: advice `find-file' to test if FILENAME is remote and pply in absolute mode in that case

(defun find-file-absolute (&optional filename wildcards)
  "`file-file' that always asume file-name is absolute.

This behaves differently to to the default relative behaviour.

Useful when pasting complete TRAMP paths while already on a remote host."
  (interactive)
  (let ((filename (or filename (read-string "Filename: ")))
        (default-directory "/"))
    (find-file filename wildcards)))



;; multi-scratch

(use-package multi-scratch
  :disabled
  :init
  (setq multi-scratch-buffer-name "scratch")
  :config
  (defalias '_msn 'multi-scratch-new))



;; WIP PUSHBULLET PACKAGE

(use-package pushbullet-api
  :load-path "/home/eigen/.emacs.d/plugins/pushbullet-reader")

(use-package pushbullet
  :load-path "/home/eigen/.emacs.d/plugins/pushbullet-reader"
  :after pushbullet-api)



;; auto-determine thing at point

(defun prf/thing-at-point (THING)
  "wrapper around thing-at-point to support other types"
  (cond
   ((string= THING 'url)
    (substring  (thing-at-point 'url) 7))
   (t (thing-at-point THING))))


;; https://www.emacswiki.org/emacs/SyntaxAtPoint

;; this is hackish but way easier than defining a new THING the standard way.
(defun dotted-symbol-at-point ()
  (with-syntax-table (make-syntax-table (syntax-table))
    (modify-syntax-entry ?. "_")
    (thing-at-point 'symbol)))



;; don't complain about runing processes when quiting

;; (defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
;;   (noflet ((process-list ())) ad-do-it))

(defadvice save-buffers-kill-emacs (around no-query-kill-emacs activate)
  (cl-flet ((process-list ())) ad-do-it))



;; faster shell-command

(defun fast-shell-command (command &optional output-buffer)
  "A fast, no brainer, fork of shell-command, whithout shell-mode."
  (interactive
   (read-shell-command "Shell command: " nil nil
                       (let ((filename
                              (cond
                               (buffer-file-name)
                               ((eq major-mode 'dired-mode)
                                (dired-get-filename nil t)))))
                         (and filename (file-relative-name filename)))))

  (unless output-buffer
    (setq output-buffer "*Fast Shell Command Output*"))
  (switch-to-buffer-other-window output-buffer)
  (start-process command output-buffer
                 "/bin/sh" "-c" command))



;; helper to move cursor at position (line:col)

(defun pos-at-line-col (l c)
  (goto-char (point-min))
  (forward-line (- l 1))
  (move-to-column c)
  (point))

(defun pos-at-line-col-joined (l-c &optional separator)
  (unless separator
    (setq separator ":"))
  (let* ((l-c (s-split separator l-c))
         (l (string-to-number (car l-c)))
         (c (string-to-number (car (cdr l-c)))))
    (pos-at-line-col l c)))



;; set a local-buffer key

(defun perf/buffer-local-set-key (key command)
  (interactive "KSet key buffer-locally: \nCSet key %s buffer-locally to command: ")
  (let ((oldmap (current-local-map))
        (newmap (make-sparse-keymap)))
    (when oldmap
      (set-keymap-parent newmap oldmap))
    (define-key newmap key command)
    (use-local-map newmap)))



;; VAGRANT local / ssh toggle

;; either do a `vagrant global-status' or look at file ~/.vagrant.d/data/machine-index/index

(defun prf/tramp/vagrant/vagrant-server-p (&optional path)
  (setq path (if path path (buffer-file-name)))
  (let (vec method user host path)
    (setq vec (tramp-dissect-file-name path))
    (setq vec (prf/tramp/vec/with-new-localname vec "/vagrant"))
    (file-directory-p (prf/tramp/vec/undissect vec))))




(provide 'init-test1)

(prf/require-plugin 'noflet)

(when (prf/require-plugin 'multi-scratch nil 'no-error)
  (setq multi-scratch-buffer-name "scratch")
  (defalias '_msn 'multi-scratch-new)
  )

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
;; URL encode / decode

(require 'url-util)

(defun prf/url/encode (begin end)
  "URL-encode selected region."
  (interactive "r")
  (atomic-change-group
    (let ((txt (delete-and-extract-region begin end)))
      (insert (url-encode-url txt)))))
(defun prf/url/decode (begin end)
  "URL-decode selected region."
  (interactive "r")
  (atomic-change-group
    (let ((txt (delete-and-extract-region begin end)))
      (insert (decode-coding-string (url-unhex-string txt) 'utf-8)))))


;; -------------------------------------------------------------------------
;; smart TRAMP dir track by parsing PS1
;; examples:
;; - \u@\h \w>
;; - ${debian_chroot:+($debian_chroot)}\u@\h:\w\$

;; (defun prf/tramp/extract-dir-from-prompt (prompt ps1)
;;   )

(defun prf/tramp/ps1-to-regex (ps1)
  ;; get position and value of user part
  (when (string-match "^.*(\\u).*$" ps1)
    (match-string 1 path))
  ;; get position and value of directory
  (when (string-match "^.*(\\d).*$" ps1)
    (match-string 1 path))
  )

;; -------------------------------------------------------------------------

;; make helm and lusty more snappy
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)



(provide 'init-test1)

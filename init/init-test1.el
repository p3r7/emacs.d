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

;; make helm and lusty more snappy
;; http://bling.github.io/blog/2016/01/18/why-are-you-changing-gc-cons-threshold/

(defun my-minibuffer-setup-hook ()
  (setq gc-cons-threshold most-positive-fixnum))

(defun my-minibuffer-exit-hook ()
  (setq gc-cons-threshold 800000))

(add-hook 'minibuffer-setup-hook #'my-minibuffer-setup-hook)
(add-hook 'minibuffer-exit-hook #'my-minibuffer-exit-hook)



(provide 'init-test1)

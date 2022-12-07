
(require 's)
(require 'rx)



;; KUBE PORCELAIN

;; NB: using forked version that does propper lookup on init when accessing a remote (TRAMP) instance
(use-package kubel
  :load-path "~/.emacs.d/plugins-spe/kubel-20220104.2320")



;; SHELL FONT LOCK

(with-eval-after-load 'shx
  ;; - ns
  (setq prf/kube/instance-name-re (rx (one-or-more (any "a-z" "A-Z" "0-9" "-"))))
  (--map
   (add-to-list 'shx-shell-mode-font-locks (list (concat it " .*\\<-n *\\(\\<" prf/kube/instance-name-re "\\>\\) .*\\'") 1 'font-lock-function-name-face))
   '("kubectl" "k" "helm"))
  ;; - resource
  (--map
   (add-to-list 'shx-shell-mode-font-locks (list (concat it " .*\\<get *\\(\\<" prf/kube/instance-name-re "\\>\\).*") 1 'font-lock-keyword-face))
   '("kubectl" "k"))
  (--map
   (add-to-list 'shx-shell-mode-font-locks (list (concat it " .*\\<describe *\\(\\<" prf/kube/instance-name-re "\\>\\).*") 1 'font-lock-keyword-face))
   '("kubectl" "k"))
  (--map
   (add-to-list 'shx-shell-mode-font-locks (list (concat it " .*\\<label *\\(\\<" prf/kube/instance-name-re "\\>\\).*") 1 'font-lock-keyword-face))
   '("kubectl" "k"))
  (--map
   (add-to-list 'shx-shell-mode-font-locks (list (concat it " .*\\<delete *\\(\\<" prf/kube/instance-name-re "\\>\\).*") 1 'font-lock-keyword-face))
   '("kubectl" "k"))
  (--map
   (add-to-list 'shx-shell-mode-font-locks (list (concat it " .*\\(\\<logs\\>\\).*") 1 'font-lock-keyword-face))
   '("kubectl" "k"))
  ;; - instance
  (--map
   (add-to-list 'shx-shell-mode-font-locks (list (concat it " .*\\<get *\\<" prf/kube/instance-name-re "\\> \\(\\<" prf/kube/instance-name-re "\\>\\).*") 1 'font-lock-variable-name-face))
   '("kubectl" "k"))
  (--map
   (add-to-list 'shx-shell-mode-font-locks (list (concat it " .*\\<describe *\\<" prf/kube/instance-name-re "\\> \\(\\<" prf/kube/instance-name-re "\\>\\).*") 1 'font-lock-variable-name-face))
   '("kubectl" "k"))
  (--map
   (add-to-list 'shx-shell-mode-font-locks (list (concat it " .*\\<label *\\<" prf/kube/instance-name-re "\\> \\(\\<" prf/kube/instance-name-re "\\>\\).*") 1 'font-lock-variable-name-face))
   '("kubectl" "k"))
  (--map
   (add-to-list 'shx-shell-mode-font-locks (list (concat it " .*\\<delete *\\<" prf/kube/instance-name-re "\\> \\(\\<" prf/kube/instance-name-re "\\>\\).*") 1 'font-lock-variable-name-face))
   '("kubectl" "k"))
  (--map
   (add-to-list 'shx-shell-mode-font-locks (list (concat it " .*\\<logs *\\(\\<" prf/kube/instance-name-re "\\>\\).*") 1 'font-lock-variable-name-face))
   '("kubectl" "k"))
  (add-to-list 'shx-shell-mode-font-locks (list (concat "helm .*\\<uninstall *\\(\\<" prf/kube/instance-name-re "\\>\\).*") 1 'font-lock-variable-name-face)))



;; DIRECT SHELL INTEGRATIONS

(with-eval-after-load 'friendly-shell-command

  (setq kubectl-kls-command-rx
        (rx bol
            (literal "kls")))

  (setq kubectl-command-with-ns-rx
        (rx bol
            (or (literal "k")
                (literal "kubectl"))
            (one-or-more " ")
            (literal "-n")
            (one-or-more " ")
            (group (one-or-more (any "a-z" "A-Z" "0-9" "-")))
            (one-or-more " ")))

  (setq kubectl-command-all-ns-rx
        (rx bol
            (or (literal "k")
                (literal "kubectl"))
            (one-or-more (any "a-z" "A-Z" "0-9" "-" " "))
            (literal "get")
            (group (one-or-more (any "a-z" "A-Z" "0-9" "-" " ")))
            (literal "-A")              ; <- important bit
            (one-or-more " ")))

  (setq kubectl-command-with-resource-rx
        (rx bol
            (or (literal "k")
                (literal "kubectl"))
            (one-or-more (any "a-z" "A-Z" "0-9" "-" " "))
            (or (literal "get")
                ;; (literal "describe")
                ;; (literal "label")
                ;; (literal "delete")
                )
            (one-or-more " ")
            (group (one-or-more (any "a-z" "A-Z" "0-9" "-")))))

  (defun expand-kube-resource-abrev (resource-abrev)
    (pcase resource-abrev
      ("pod" "pod")
      ("po" "pod")
      ("deployment" "deployment")
      ("deploy" "deployment")
      ("cronjob" "cronjob")
      ("cj" "cronjob")
      ("daemonset" "daemonset")
      ("ds" "daemonset")
      ("service" "service")
      ("svc" "service")
      ("node" "node")
      ("nodes" "node")
      ("pv" "pv")
      ("pvc" "pvc")
      (t (user-error (concat "Unexpected kube resource or resource abrev " resource-abrev)))))


  ;; actions

  (defun kubectl-logs (ns resource object)
    (unless (string= resource "pod")
      (user-error "Can only get logs of pod objects"))
    (friendly-shell-command-async (concat "kubectl -n " ns " logs " object) :output-buffer (concat "*kubectl - logs - " ns "/" object "*")))

  (defun kubectl-logs-tail (ns resource object &optional nb-lines)
    (unless (string= resource "pod")
      (user-error "Can only get logs of pod objects"))
    (let ((nb-lines (or nb-lines 100)))
      (friendly-shell-command-async (concat "kubectl -n " ns " logs " object " --tail " (number-to-string nb-lines)) :output-buffer (concat "*kubectl - logs - " ns "/" object "*"))))

  (defun kubectl-get-yaml (ns resource object)
    (friendly-shell-command-async (concat "kubectl -n " ns " get " resource " " object " -o yaml") :output-buffer (concat "*kubectl - yaml " resource "/" ns "/" object "*")))

  (defun kubectl-describe (ns resource object)
    (friendly-shell-command-async (concat "kubectl -n " ns " describe " resource " " object) :output-buffer (concat "*kubectl - desc " resource "/" ns "/" object "*")))

  (defun kubectl-delete (ns resource object)
    (when (y-or-n-p (concat "Really delete " resource "/" object " in ns " ns " ?"))
      (friendly-shell-command-async (concat "kubectl -n " ns " delete " resource " " object) :output-buffer (concat "*kubectl - delete - " resource "/" ns "/" object "*"))))

  ;; similar to `kubel-exec-shell-pod'
  (defun kubectl-shell (ns resource object)
    (unless (string= resource "pod")
      (user-error "Cannot jump into a resource other than a pod"))
    (let* ((dir-prefix (or
                        (when (tramp-tramp-file-p default-directory)
                          (with-parsed-tramp-file-name default-directory nil
                            (format "%s%s:%s@%s|" (or hop "") method user host))) ""))
           (containers (kube--get-pod-containers ns object))
           (container (if (equal (length containers) 1)
                          (car containers)
                        (completing-read "Select container: " containers))))
      (friendly-shell
       :path (format "/%skubectl:%s@%s:/" dir-prefix container object)
       :buffer-name (concat "kubectl - sh - " resource "/" ns "/" object))))


  ;; stateless version of `kubel--get-containers'
  (defun kube--get-pod-containers (ns pod)
    (split-string
     (kubel--exec-to-string
      (format "kubectl -n %s get pod %s -o jsonpath='{.spec.%s[*].name}'" ns pod "containers")) " "))

  ;; shell-mode command parsing

  ;; TODO: make generic
  (defun get-previous-kubectl-shell-command ()
    (save-excursion
      (comint-previous-prompt 1)        ; NB: slow af!
      ;; (thing-at-point 'line)
      (s-trim (buffer-substring (point) (line-end-position)))))

  (defun ns-from-kubectl-command (command)
    (cond
     ((s-matches-p kubectl-kls-command-rx command)
      nil)
     ((s-matches-p kubectl-command-all-ns-rx command)
      nil)
     ((s-match-strings-all kubectl-command-with-ns-rx command)
      (cadr (car (s-match-strings-all kubectl-command-with-ns-rx command))))
     (t
      (user-error (concat "Failed extracting ns from command: " command)))))

  (defun resource-from-kubectl-command (command)
    (cond
     ((s-matches-p kubectl-kls-command-rx command)
      "pod")
     ((s-matches-p kubectl-command-with-resource-rx command)
      (expand-kube-resource-abrev (cadr (car (s-match-strings-all kubectl-command-with-resource-rx command)))))
     (t
      (user-error (concat "Failed extracting resource from command: " command)))))

  (defun ctx-from-kubectl-command (command)
    (let ((ns (ns-from-kubectl-command command))
          (resource (resource-from-kubectl-command command)))
      (list resource ns)))

  (defun get-previous-kubectl-shell-command-context ()
    (let ((command (get-previous-kubectl-shell-command)))
      (unless (--any (s-starts-with? it command) '("k " "kubectl " "kls"))
        (user-error "Not a valid kube command"))
      (ctx-from-kubectl-command command)))


  ;; shell-mode output parsing

  (defun kubectl-object-at-point-shell-mode (ns-maybe)
    (let* ((line (thing-at-point 'line))
           (split-line (s-split " " line 't)))
      (let ((ns (or ns-maybe (car split-line)))
            (object (if ns-maybe (car split-line) (cadr split-line))))
        (list ns object))))


  ;; action commands

  (defun kubectl-action-on-object-at-point (action-fn)
    (cond
     ((eq major-mode 'shell-mode)
      (let* ((ctx (get-previous-kubectl-shell-command-context))
             (resource (car ctx))
             (ns-maybe (cadr ctx))
             (namespaced-object (kubectl-object-at-point-shell-mode ns-maybe))
             (ns (car namespaced-object))
             (object (cadr namespaced-object)))
        (funcall action-fn ns resource object)
        ;; (message "%s" (list action-fn ns resource object))
        ))
     (t (user-error "Unsupported operation for current buffer"))))

  (defun kubectl-logs-at-point ()
    (interactive)
    (kubectl-action-on-object-at-point #'kubectl-logs))

  (defun kubectl-shell-at-point ()
    (interactive)
    (kubectl-action-on-object-at-point #'kubectl-shell))

  (defun kubectl-logs-tail-at-point ()
    (interactive)
    (kubectl-action-on-object-at-point #'kubectl-logs-tail))

  (defun kubectl-yaml-at-point ()
    (interactive)
    (kubectl-action-on-object-at-point #'kubectl-get-yaml))

  (defun kubectl-describe-at-point ()
    (interactive)
    (kubectl-action-on-object-at-point #'kubectl-describe))

  (defun kubectl-delete-at-point ()
    (interactive)
    (kubectl-action-on-object-at-point #'kubectl-delete)))

(with-eval-after-load 'hydra
  (defhydra hydra-kube (:color blue)
    "kubernetes"
    ("l" kubectl-logs-tail-at-point "logs (last)")
    ("L" kubectl-logs-at-point "logs (all)")
    ("S" kubectl-shell-at-point "shell")
    ("y" kubectl-yaml-at-point "yaml def")
    ("i" kubectl-describe-at-point "describe")
    ("K" kubectl-delete-at-point "delete")
    ("g" nil "cancel")))




(provide 'init-kubernetes)

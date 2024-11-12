
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

;; (with-eval-after-load 'friendly-shell-command
(progn

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
      ("job" "job")
      ("cronjob" "cronjob")
      ("cj" "cronjob")
      ("configmap" "configmap")
      ("cm" "configmap")
      ("daemonset" "daemonset")
      ("ds" "daemonset")
      ("secret" "secret")
      ("service" "service")
      ("svc" "service")
      ("ingress" "ingress")
      ("ing" "ingress")
      ("node" "node")
      ("nodes" "node")
      ("pv" "pv")
      ("pvc" "pvc")
      ("statefulset" "sts")
      ("sts" "sts")
      (_ (user-error (concat "Unexpected kube resource or resource abrev " resource-abrev)))))

  (defun kubectl-fq-name (ns resource object)
    (concat resource "/" ns "/" object))


  ;; shell commands

  (defun kubectl-logs-shell-command (ns resource object &optional nb-lines)
    (concat "kubectl -n " ns " logs " resource "/" object
            (if nb-lines (concat " --tail " (number-to-string nb-lines)) "")))

  (defun kubectl-get-yaml-shell-command (ns resource object)
    (concat "kubectl -n " ns " get " resource " " object " -o yaml"))

  (defun kubectl-describe-shell-command (ns resource object)
    (concat "kubectl -n " ns " describe " resource " " object " -o yaml"))

  (defun kubectl-delete-shell-command (ns resource object &optional force)
    (concat "kubectl -n " ns " delete " resource " " object
            (if force " --force --grace-period=0" "")))


  ;; emacs commands

  (defun kubectl-delete-force-shell-command (ns resource object)
    (concat "kubectl -n " ns " delete " resource " " object))

  (defun kubectl-logs (ns resource object)
    (unless (string= resource "pod")
      (user-error (concat "Cannot get logs of " resource " objects")))
    (friendly-shell-command-async (kubectl-logs-shell-command ns resource object)
                                  :output-buffer (concat "*kubectl - logs - " (kubectl-fq-name ns resource object) "*")))

  (defun kubectl-logs-tail (ns resource object &optional nb-lines)
    (unless (string= resource "pod")
      (user-error "Can only get logs of pod objects"))
    (let ((nb-lines (or nb-lines 100)))
      (friendly-shell-command-async (kubectl-logs-shell-command ns resource object nb-lines)
                                    :output-buffer (concat "*kubectl - logs - " (kubectl-fq-name ns resource object) "*"))))

  (defun kubectl-get-yaml (ns resource object)
    (let ((buff-name (concat "*kubectl - yaml " (kubectl-fq-name ns resource object) "*")))
      (friendly-shell-command-async (kubectl-get-yaml-shell-command ns resource object)
                                    :output-buffer buff-name
                                    :callback `(lambda ()
                                                 (let ((buff (get-buffer ,buff-name)))
                                                   (when (buffer-live-p buff)
                                                     (save-excursion
                                                       (set-buffer buff)
                                                       (goto-char (point-min))
                                                       (when (re-search-forward (rx bol
                                                                                    (literal "Connection to ")
                                                                                    (one-or-more (any "a-z" "A-Z" "0-9" "-" "_" "."))
                                                                                    (literal " closed.")
                                                                                    eol)
                                                                                nil t)
                                                         (replace-match ""))
                                                       (yaml-mode))))))))

  (defun kubectl-describe (ns resource object)
    (friendly-shell-command-async (kubectl-describe-shell-command ns resource object)
                                  :output-buffer (concat "*kubectl - desc " (kubectl-fq-name ns resource object) "*")))

  (defun kubectl-delete (ns resource object)
    (when (y-or-n-p (concat "Really delete " resource "/" object " in ns " ns " ?"))
      (friendly-shell-command-async (kubectl-delete-shell-command ns resource object)
                                    :output-buffer (concat "*kubectl - delete - " (kubectl-fq-name ns resource object) "*"))))

  (defun kubectl-delete-force (ns resource object)
    (when (y-or-n-p (concat "Really force delete " resource "/" object " in ns " ns " ?"))
      (friendly-shell-command-async (kubectl-delete-shell-command ns resource object 't)
                                    :output-buffer (concat "*kubectl - delete (force) - " (kubectl-fq-name ns resource object) "*"))))

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
       :buffer-name (concat "kubectl - sh - " (kubectl-fq-name ns resource object)))))


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

  (defun kubectl-object-in-line-shell-mode (line ns-maybe resource)
    (let* ((split-line (s-split " " line 't))
           (ns (or ns-maybe (car split-line)))
           (object (if ns-maybe (car split-line) (cadr split-line))))
      (list ns resource object)))

  (defun kubectl-object-at-point-shell-mode (ns-maybe resource)
    (let ((line (thing-at-point 'line)))
      (kubectl-object-in-line-shell-mode line ns-maybe resource)))

  (defun get-region-whole-lines ()
    "Return the content of the complete lines between the mark and point."
    (interactive)
    ;; Ensure the region is active and the mark is set
    (when (not (region-active-p))
      (user-error "No active region"))
    ;; Determine the start and end of the region
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      ;; Check the positions of mark and point
      (if (> (point) (mark))
          (setq start (save-excursion
                        (goto-char (mark))
                        (line-beginning-position))
                end (save-excursion
                      (goto-char (point))
                      (line-end-position)))
        (setq end (save-excursion
                    (goto-char (mark))
                    (line-end-position))
              start (save-excursion
                      (goto-char (point))
                      (line-beginning-position))))
      ;; Extract the string from the buffer
      (buffer-substring-no-properties start end)))

  (defun kubectl-object-in-region-shell-mode ()
    (interactive)
    (let* ((ctx (get-previous-kubectl-shell-command-context))
           (resource (car ctx))
           (ns-maybe (cadr ctx))
           (lines (split-string (get-region-whole-lines) "\n")))
      (--map (kubectl-object-in-line-shell-mode it ns-maybe resource) lines)))


  ;; action commands

  (defun kubectl-action-on-object-at-point (action-fn)
    (cond
     ((eq major-mode 'shell-mode)
      (let* ((ctx (get-previous-kubectl-shell-command-context))
             (resource (car ctx))
             (ns-maybe (cadr ctx))
             (namespaced-object (kubectl-object-at-point-shell-mode ns-maybe resource))
             (ns (car namespaced-object))
             (object (nth 2 namespaced-object)))
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
    (kubectl-action-on-object-at-point #'kubectl-delete))

  (defun kubectl-delete-force-at-point ()
    (interactive)
    (kubectl-action-on-object-at-point #'kubectl-delete-force)))

(with-eval-after-load 'hydra
  (defhydra hydra-kube (:color blue)
    "kubernetes"
    ("l" kubectl-logs-tail-at-point "logs (last)")
    ("L" kubectl-logs-at-point "logs (all)")
    ("S" kubectl-shell-at-point "shell")
    ("y" kubectl-yaml-at-point "yaml def")
    ("i" kubectl-describe-at-point "describe")
    ("k" kubectl-delete-at-point "delete")
    ("K" kubectl-delete-force-at-point "delete (f)")
    ("g" nil "cancel")))




(provide 'init-kubernetes)

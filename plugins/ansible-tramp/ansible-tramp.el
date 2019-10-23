
(require 'json)
(require 'request-deferred)
(require 's)
(require 'subr-x)
(require 'buffer-grid)


;; (when (executable-find "ansible")
;;   )

;; TODO: maybe reparse the facts tree into a more user-friendly object?
;; TODO: thing at point ext, when over hostname:
;;  - open in dired
;;  - open in shell-mode
;;  - add bookmark (pscp)

;; ------------------------------------------------------------------------
;; VARS

(defvar ansible-tramp-ansible-bin "ansible")
(defvar ansible-tramp-ansible-user nil)
(defvar ansible-tramp-remote-ansible-cnnx nil)
(defvar ansible-tramp-prefer-remote nil)

(defvar ansible-tramp-inventory-http-url nil)
(defvar ansible-tramp-inventory-cache nil)
(defvar ansible-tramp-inventory-cache-set-time nil)


;; ------------------------------------------------------------------------
;; PUBLIC FUNCS

;; REVIEW: should be able to force local cnxx even if default is prefer-remote
(defun ansible-tramp-get-facts-for-host (host &optional ansible-bin ansible-user remote-ansible-cnnx)
  (let* ((prefer-remote (if remote-ansible-cnnx 't ansible-tramp-prefer-remote))
	 (remote-ansible-cnnx (if remote-ansible-cnnx remote-ansible-cnnx ansible-tramp-remote-ansible-cnnx))
	 (raw-facts))
    (setq raw-facts
	  (if prefer-remote
	      (ansible-tramp--remote-exec-module-setup host remote-ansible-cnnx ansible-bin ansible-user)
	    (ansible-tramp--exec-module-setup host ansible-bin ansible-user)))
    (ansible-tramp--parse-task-setup-output raw-facts host)))


(defun ansible-tramp-get-fact-for-host (host factPath &optional ansible-bin ansible-user remote-ansible-cnnx)
  (let ((facts))
    (setq facts (ansible-tramp-get-facts-for-host host ansible-bin ansible-user remote-ansible-cnnx))
    (prf/gethash-recursive-dot-bucket facts factPath)))


(defun ansible-tramp-get-default-ipv4-for-host (host &optional ansible-bin ansible-user remote-ansible-cnnx)
  (ansible-tramp-get-fact-for-host host "ansible_default_ipv4.address" &optional ansible-bin ansible-user remote-ansible-defun))


(defun ansible-tramp-get-inventory-hosts (&optional inventory-http-url)
  (if ansible-tramp-inventory-cache
      (prf/gethash-recursive ansible-tramp-inventory-cache "_meta" "hostvars")
    (message "Reloading inventory cache, retry later")
    (ansible-tramp-set-inventory-cache-http inventory-http-url)))


(defun ansible-tramp-get-inventory-hostnames (&optional inventory-http-url)
  (let ((inventory-hosts-hash-table (ansible-tramp-get-inventory-hosts inventory-http-url)))
    (when inventory-hosts-hash-table
      (hash-table-keys inventory-hosts-hash-table))))


(defun ansible-tramp-get-inventory-vars-for-host (host &optional inventory-http-url)
  (if ansible-tramp-inventory-cache
      (prf/gethash-recursive ansible-tramp-inventory-cache "_meta" "hostvars" host)
    (message "Reloading inventory cache, retry later")
    (ansible-tramp-set-inventory-cache-http inventory-http-url)))


(defun ansible-tramp-get-inventory-var-for-host (host varname &optional inventory-http-url)
  (let ((host-vars))
    (setq host-vars (ansible-tramp-get-inventory-vars-for-host host inventory-http-url))
    (when host-vars
      (gethash varname host-vars))))

(defun ansible-tramp-get-inventory-address-for-host (host &optional inventory-http-url)
  (ansible-tramp-get-inventory-var-for-host host "ansible_host" inventory-http-url))


(defun ansible-tramp-set-inventory-cache-http (&optional inventory-http-url)
  (interactive)
  (ansible-tramp--http-inventory-callback
   '(lambda (response)
      (setq ansible-tramp-inventory-cache (request-response-data response)
	    ansible-tramp-inventory-cache-set-time (current-time))
      (message "Ansible Inventory cache set"))))


(defun ansible-tramp-clear-inventory-cache ()
  (interactive)
  (setq ansible-tramp-inventory-cache nil
	ansible-tramp-inventory-cache-set-time nil))


;; ------------------------------------------------------------------------
;; HELM INTEGRATION

;; http://kitchingroup.cheme.cmu.edu/blog/2015/01/30/More-adventures-in-helm-more-than-one-action/

(with-eval-after-load "helm"

  (setq ansible-tramp-helm-source
        (helm-build-sync-source "Connect to Ansible host"
          :candidates #'ansible-tramp-get-inventory-hostnames
          :fuzzy-match t
          :action '(("shell" .
                     (lambda (candidate)
                       (let* ((marked-candidates (helm-marked-candidates))
                              (nb-marked-candidates (length marked-candidates))
                              buf-list)
                         (mapc
                          (lambda (e)
                            (add-to-list 'buf-list (prf/tramp/remote-shell (ansible-tramp-get-inventory-address-for-host e)) 't))
                          marked-candidates)
                         (when (< 1 nb-marked-candidates)
                           (buffer-grid-diplay buf-list 't)))))
                    ("dired" .
                     (lambda (candidate)
                       (let ((host-address (ansible-tramp-get-inventory-address-for-host candidate)))
                         (prf/tramp/remote-shell host-address ))))
                    ("debug" .
                     (lambda (candidate)
                       (let ((hostname candidate)
                             (host-address (ansible-tramp-get-inventory-address-for-host candidate)))
                         (message-box "selected: %s (%s)" hostname host-address)))))))

  (defun helm-ansible-inventory-host-connect ()
    (interactive)
    (let ((helm-candidate-number-limit 10000))
      (helm :sources '(ansible-tramp-helm-source)
            :input (let ((tap (thing-at-point 'symbol)))
                     (when (member tap (ansible-tramp-get-inventory-hostnames)) tap))
            :buffer "*helm Tramp Shell Ansible*"))))


;; ------------------------------------------------------------------------
;; PRIVATE GENERIC FUNCS

;; https://emacs.stackexchange.com/a/3208
(defun prf/gethash-recursive (hashtable &rest keys)
  "Recursively find KEYs in HASHTABLE."
  (while keys
    (setq hashtable (gethash (pop keys) hashtable)))
  hashtable)


(defun prf/gethash-recursive-dot-bucket (hashtable keys-path)
  "Recursively find KEYs in HASHTABLE."
  (let ((keys (split-string keys-path ".")))
    (prf/gethash-recursive hashtable keys)))


;; ------------------------------------------------------------------------
;; PRIVATE FUNCS: ANSIBLE INVENTORY HTTP API
;; NB: those are async using defer

(defun perf/test/message-inventory (&optional inventory-http-url)
  (ansible-tramp--http-inventory-callback
   '(lambda (response)
      (message "Got: %S" (request-response-data response)))))

(defun ansible-tramp--http-inventory-callback (callback &optional inventory-http-url)

  (setq inventory-http-url (if inventory-http-url inventory-http-url ansible-tramp-inventory-http-url))
  (when (not inventory-http-url)
    (error "Both arg inventory-http-url and var ansible-tramp-inventory-http-url are empty"))

  (deferred:$
    (request-deferred inventory-http-url
    ;; (request-deferred ansible-tramp-inventory-http-url
		      :parser (lambda ()
				(let ((json-object-type 'hash-table)
				      (json-array-type 'list)
				      (json-key-type 'string))
				  (json-read))))
    (deferred:nextc it
      callback)))
;; https://tkf.github.io/emacs-request/manual.html


;; ------------------------------------------------------------------------
;; PRIVATE FUNCS: ANSIBLE CLI

(defun ansible-tramp--build-ansible-cmd (host ansible-module &optional ansible-bin ansible-user)
  (let* ((ansible-bin (if ansible-bin ansible-bin ansible-tramp-ansible-bin))
	 (ansible-user (if ansible-user ansible-user ansible-tramp-ansible-user))
	 (ansible-cmd (concat ansible-bin " " host " -m " ansible-module)))
    (when ansible-user
      (setq ansible-cmd (concat ansible-cmd " -u " ansible-user)))
    ansible-cmd))

(defun ansible-tramp--build-ansible-cmd-setup (host &optional ansible-bin ansible-user)
  (setq ansible-bin (if ansible-bin ansible-bin ansible-tramp-ansible-bin)
	ansible-user (if ansible-user ansible-user ansible-tramp-ansible-user))
  (ansible-tramp--build-ansible-cmd host "setup" ansible-bin ansible-user))


(defun ansible-tramp--remote-exec-module-setup (host remote-ansible-cnnx &optional ansible-bin ansible-user)
  (let ((ansible-cmd (ansible-tramp--build-ansible-cmd-setup host ansible-bin ansible-user)))
    (prf/tramp/remote-shell-command-to-string remote-ansible-cnnx ansible-cmd)))


(defun ansible-tramp--exec-module-setup (host &optional ansible-bin ansible-user)
  (let ((ansible-cmd (ansible-tramp--build-ansible-cmd-setup host ansible-bin ansible-user)))
    (shell-command-to-string ansible-cmd)))


(defun ansible-tramp--parse-task-setup-output (rawRes host)
  ;; NB: in format: <HOSTNAME> | <STATUS> => <JSON>
  (when (s-starts-with? (concat host " | SUCCESS => ") rawRes)
    (setq rawRes (s-replace (concat host " | SUCCESS => ") "" rawRes))
    (let ((json-object-type 'hash-table)
	  (json-array-type 'list)
	  (json-key-type 'string))
      (gethash "ansible_facts" (json-read-from-string rawRes)))))


(provide 'ansible-tramp)



;; TODO: maybe reparse the facts tree into a more user-friendly object?
;; TODO: thing at point ext, when over hostname:
;;  - open in dired
;;  - open in shell-mode
;;  - add bookmark (pscp)


;; REQUIRES

(require 'json)
(require 'request-deferred)
(require 's)
(require 'subr-x)
(require 'buffer-grid)



;; VARS

(defvar ansible-tramp-ansible-bin "ansible" "Ansible exec name / location")
(defvar ansible-tramp-ansible-user nil "Ansible user to use for module execution.")
(defvar ansible-tramp-remote-ansible-cnnx nil)
(defvar ansible-tramp-prefer-remote nil "If 't and `ansible-tramp-remote-ansible-cnnx' is set, would use this connection even if has local ansible install")

(defvar ansible-tramp-inventory-http-url nil "HTTP URL to retrieve Ansible Inventory from")
(defvar ansible-tramp-inventory-cache nil "Cache for Ansible Inventory")
(defvar ansible-tramp-inventory-cache-set-time nil "Timestamp at which `ansible-tramp-inventory-cache' last got set.")



;; PUBLIC FUNCS

;; REVIEW: should be able to force local cnxx even if default is prefer-remote

(defun ansible-tramp-get-facts-for-host (host &optional ansible-bin ansible-user remote-ansible-cnnx)
  "Returns facts for HOST, gotten by running Ansible module \"setup\"."
  (let* ((prefer-remote (if remote-ansible-cnnx 't ansible-tramp-prefer-remote))
	 (remote-ansible-cnnx (or remote-ansible-cnnx ansible-tramp-remote-ansible-cnnx))
	 (raw-facts))
    (setq raw-facts
	  (if prefer-remote
	      (ansible-tramp--remote-exec-module-setup host remote-ansible-cnnx ansible-bin ansible-user)
	    (ansible-tramp--exec-module-setup host ansible-bin ansible-user)))
    (ansible-tramp--parse-task-setup-output raw-facts host)))


(defun ansible-tramp-get-fact-for-host (host fact-path &optional ansible-bin ansible-user remote-ansible-cnnx)
  "Returns value of fact at FACT-PATH for HOST, gotten by running Ansible module \"setup\".
FACT-PATH should be a string in dot-bucket format."
  (let ((facts))
    (setq facts (ansible-tramp-get-facts-for-host host ansible-bin ansible-user remote-ansible-cnnx))
    (prf/gethash-recursive-dot-bucket facts fact-path)))


(defun ansible-tramp-get-default-ipv4-for-host (host &optional ansible-bin ansible-user remote-ansible-cnnx)
  "Returns value of fact \"ansible_default_ipv4.address\" for HOST, gotten by running Ansible module \"setup\"."
  (ansible-tramp-get-fact-for-host host "ansible_default_ipv4.address" &optional ansible-bin ansible-user remote-ansible-defun))


(defun ansible-tramp-get-inventory-hosts (&optional inventory-http-url)
  "Attempts retrieving list of hosts in Ansible inventory.
First looks up local cache `ansible-tramp-inventory-cache' if not empty or not too old.
If not available, returns nil but tries reloading cache via an async API call (see `ansible-tramp-set-inventory-cache-http')."
  (if ansible-tramp-inventory-cache
      (prf/gethash-recursive ansible-tramp-inventory-cache "_meta" "hostvars")
    (message "Reloading inventory cache, retry later")
    (ansible-tramp-set-inventory-cache-http inventory-http-url)))


(defun ansible-tramp-get-inventory-hostnames (&optional inventory-http-url)
  "Attempts retrieving list of hostnames in Ansible inventory.
First looks up local cache `ansible-tramp-inventory-cache' if not empty or not too old.
If not available, returns nil but tries reloading cache via an async API call (see `ansible-tramp-set-inventory-cache-http')."
  (let ((inventory-hosts-hash-table (ansible-tramp-get-inventory-hosts inventory-http-url)))
    (when inventory-hosts-hash-table
      (hash-table-keys inventory-hosts-hash-table))))


(defun ansible-tramp-get-inventory-vars-for-host (host &optional inventory-http-url)
  "Returns inventory vars for HOST."
  (if ansible-tramp-inventory-cache
      (prf/gethash-recursive ansible-tramp-inventory-cache "_meta" "hostvars" host)
    (message "Reloading inventory cache, retry later")
    (ansible-tramp-set-inventory-cache-http inventory-http-url)))


(defun ansible-tramp-get-inventory-var-for-host (host varname &optional inventory-http-url)
  "Returns value of inventory var VARNAME for HOST."
  (let ((host-vars))
    (setq host-vars (ansible-tramp-get-inventory-vars-for-host host inventory-http-url))
    (when host-vars
      (gethash varname host-vars))))


(defun ansible-tramp-get-inventory-address-for-host (host &optional inventory-http-url)
  "Returns value of inventory var \"ansible_host\" for HOST."
  (ansible-tramp-get-inventory-var-for-host host "ansible_host" inventory-http-url))


(defun ansible-tramp-set-inventory-cache-http (&optional inventory-http-url)
  "Sets cache `ansible-tramp-inventory-cache' by doing an async API call.
The latter targets either INVENTORY-HTTP-URL or `ansible-tramp-inventory-http-url'."
  (interactive)
  (ansible-tramp--http-inventory-callback
   '(lambda (response)
      (setq ansible-tramp-inventory-cache (request-response-data response)
	    ansible-tramp-inventory-cache-set-time (current-time))
      (message "Ansible Inventory cache set"))
   inventory-http-url))


(defun ansible-tramp-clear-inventory-cache ()
  "Clears cache `ansible-tramp-inventory-cache'."
  (interactive)
  (setq ansible-tramp-inventory-cache nil
	ansible-tramp-inventory-cache-set-time nil))



;; HELM INTEGRATION

(with-eval-after-load "helm"

  (defun ansible-tramp--hostname-at-point ()
    (with-syntax-table (make-syntax-table (syntax-table))
      (modify-syntax-entry ?. "_")
      (thing-at-point 'symbol)))

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
            :input (let ((tap (ansible-tramp--hostname-at-point)))
                     (when (member tap (ansible-tramp-get-inventory-hostnames)) tap))
            :buffer "*helm Tramp Shell Ansible*"))))



;; UTILS: HASHMAP

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



;; PRIVATE FUNCS: ANSIBLE INVENTORY HTTP API

;; NB: those are async using defer

(defun ansible-tramp--message-inventory (&optional inventory-http-url)
  (ansible-tramp--http-inventory-callback
   '(lambda (response)
      (message "Got: %S" (request-response-data response)))))

(defun ansible-tramp--http-inventory-callback (callback &optional inventory-http-url)
  "Launch async call of API to retrieve Ansible inventory and registers CALLBACK to be triggered on end of execution."
  (setq inventory-http-url (or inventory-http-url ansible-tramp-inventory-http-url))
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



;; PRIVATE FUNCS: ANSIBLE CLI

(defun ansible-tramp--build-ansible-cmd (host ansible-module &optional ansible-bin ansible-user)
  "Build Ansible command string to target HOST with module ANSIBLE-MODULE."
  (let* ((ansible-bin (or ansible-bin ansible-tramp-ansible-bin))
	 (ansible-user (or ansible-user ansible-tramp-ansible-user))
	 (ansible-cmd (concat ansible-bin " " host " -m " ansible-module)))
    (when ansible-user
      (setq ansible-cmd (concat ansible-cmd " -u " ansible-user)))
    ansible-cmd))

(defun ansible-tramp--build-ansible-cmd-setup (host &optional ansible-bin ansible-user)
  "Build Ansible command string to target HOST with module \"setup\"."
  (ansible-tramp--build-ansible-cmd host "setup" ansible-bin ansible-user))


(defun ansible-tramp--remote-exec-module-setup (host remote-ansible-cnnx &optional ansible-bin ansible-user)
  "Launch Ansible shell command targeting HOST with module \"setup\", at remote path REMOTE-ANSIBLE-CNNX."
  (let ((ansible-cmd (ansible-tramp--build-ansible-cmd-setup host ansible-bin ansible-user)))
    (prf/tramp/remote-shell-command-to-string remote-ansible-cnnx ansible-cmd)))

;; REVIEW: should be able to use `ansible-tramp--remote-exec-module-setup' w/ local paths as well
(defun ansible-tramp--exec-module-setup (host &optional ansible-bin ansible-user)
  (let ((ansible-cmd (ansible-tramp--build-ansible-cmd-setup host ansible-bin ansible-user)))
    (shell-command-to-string ansible-cmd)))


(defun ansible-tramp--parse-task-setup-output (raw-res host)
  "Parses output RAW-RES of \"setup\" Ansible command targeting HOST."
  ;; NB: in format: <HOSTNAME> | <STATUS> => <JSON>
  (when (s-starts-with? (concat host " | SUCCESS => ") raw-res)
    (setq raw-res (s-replace (concat host " | SUCCESS => ") "" raw-res))
    (let ((json-object-type 'hash-table)
	  (json-array-type 'list)
	  (json-key-type 'string))
      (gethash "ansible_facts" (json-read-from-string raw-res)))))




(provide 'ansible-tramp)

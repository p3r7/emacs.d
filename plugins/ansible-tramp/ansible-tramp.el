

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
(defvar ansible-tramp-cnnx nil "Path to launch ansible inventory from.
Only really usefull to set it for remote executions w/ TRAMP.
Support remote paths in form /<method>:<user>@<host>:/.")

(defvar ansible-tramp-inventory-lookup-method 'cli "Preffered method to retrieve inventory.
Either 'cli or 'url.")
(defvar ansible-tramp-inventory-http-url nil "HTTP URL to retrieve Ansible Inventory from.")
(defvar ansible-tramp-inventory-cli-exec "ansible-inventory" "Name of ansible-inventory executable.
If not in PATH env var, you should specify an absolute path to the exec.
Expected to be called with option \"--list\".")

(defvar ansible-tramp-inventory-cache nil "Cache for Ansible Inventory")
(defvar ansible-tramp-inventory-cache-set-time nil "Timestamp at which `ansible-tramp-inventory-cache' last got set.")



;; HOST FACTS

;; REVIEW: should be able to force local cnxx even if default is prefer-remote

(defun ansible-tramp-get-facts-for-host (host &optional ansible-bin ansible-user ansible-cnnx)
  "Returns facts for HOST, gotten by running Ansible module \"setup\"."
  (let* ((ansible-cnnx (or ansible-cnnx ansible-tramp-cnnx default-directory))
	 (raw-facts (ansible-tramp--exec-module-setup host ansible-cnnx ansible-bin ansible-user)))
    (ansible-tramp--parse-task-setup-output raw-facts host)))

(defun ansible-tramp-get-fact-for-host (host fact-path &optional ansible-bin ansible-user ansible-cnnx)
  "Returns value of fact at FACT-PATH for HOST, gotten by running Ansible module \"setup\".
FACT-PATH should be a string in dot-bucket format."
  (let ((facts (ansible-tramp-get-facts-for-host host ansible-bin ansible-user ansible-cnnx)))
    (prf/gethash-recursive-dot-bucket facts fact-path)))

(defun ansible-tramp-get-default-ipv4-for-host (host &optional ansible-bin ansible-user ansible-cnnx)
  "Returns value of fact \"ansible_default_ipv4.address\" for HOST, gotten by running Ansible module \"setup\"."
  (ansible-tramp-get-fact-for-host host "ansible_default_ipv4.address" &optional ansible-bin ansible-user ansible-cnnx))



;; INVENTORY

;; BUG: does systematic reload of cache even though recent
(cl-defun ansible-tramp-get-inventory-hosts (&key inventory-bin ansible-cnnx
                                                  inventory-http-url)
  "Attempts retrieving list of hosts in Ansible inventory.
First looks up local cache `ansible-tramp-inventory-cache' if not empty or not too old.
If not available, returns nil but tries reloading cache via an async API call."
  (cond
   (inventory-http-url
    (ansible-tramp-set-inventory-cache-http inventory-http-url))
   ((or inventory-bin ansible-cnnx)
    (ansible-tramp-set-inventory-cache-cli inventory-bin ansible-cnnx))
   (t
    (if (eq ansible-tramp-inventory-lookup-method 'url)
        (ansible-tramp-get-inventory-hosts-http ansible-tramp-inventory-http-url)
      (ansible-tramp-get-inventory-hosts-cli ansible-tramp-inventory-cli-exec ansible-tramp-cnnx)))))

(defun ansible-tramp-get-inventory-hosts-cli (&optional inventory-bin ansible-cnnx)
  "Attempts retrieving list of hosts in Ansible inventory.
First looks up local cache `ansible-tramp-inventory-cache' if not empty or not too old."
  (if ansible-tramp-inventory-cache
      (prf/gethash-recursive ansible-tramp-inventory-cache "_meta" "hostvars")
    (message "Reloading inventory cache, retry later")
    (ansible-tramp-set-inventory-cache-cli inventory-bin ansible-cnnx)))

(defun ansible-tramp-get-inventory-hosts-http (&optional inventory-http-url)
  "Attempts retrieving list of hosts in Ansible inventory.
First looks up local cache `ansible-tramp-inventory-cache' if not empty or not too old.
If not available, returns nil but tries reloading cache via an async API call (see `ansible-tramp-set-inventory-cache-http')."
  (if ansible-tramp-inventory-cache
      (prf/gethash-recursive ansible-tramp-inventory-cache "_meta" "hostvars")
    (message "Reloading inventory cache, retry later")
    (ansible-tramp-set-inventory-cache-http inventory-http-url)))

(defun ansible-tramp-set-inventory-cache-cli (&optional inventory-bin ansible-cnnx)
  "Sets cache `ansible-tramp-inventory-cache' by doing an async API call."
  (interactive)
  (ansible-tramp--cli-inventory-callback
   '(lambda (response)
      (setq ansible-tramp-inventory-cache (request-response-data response)
	    ansible-tramp-inventory-cache-set-time (current-time))
      (message "Ansible Inventory cache set"))
   inventory-bin ansible-cnnx))

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

(defun ansible-tramp-get-inventory-hostnames (&optional inventory-http-url)
  "Attempts retrieving list of hostnames in Ansible inventory.
First looks up local cache `ansible-tramp-inventory-cache' if not empty or not too old.
If not available, returns nil but tries reloading cache via an async API call (see `ansible-tramp-set-inventory-cache-http')."
  ;; TODO: make it generic (work w/ cli)
  ;; (let ((inventory-hosts-hash-table (ansible-tramp-get-inventory-hosts :inventory-http-url inventory-http-url)))
  (let ((inventory-hosts-hash-table (ansible-tramp-get-inventory-hosts-http inventory-http-url)))
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

(defun ansible-tramp--message-inventory-cli (&optional inventory-bin ansible-cnnx)
  (ansible-tramp--cli-inventory-callback
   '(lambda (response)
      (message "Got: %S" (request-response-data response)))))

(defun ansible-tramp--message-inventory-http (&optional inventory-http-url)
  (ansible-tramp--http-inventory-callback
   '(lambda (response)
      (message "Got: %S" (request-response-data response)))))

(defun ansible-tramp--cli-inventory-callback (callback &optional inventory-bin ansible-cnnx)
  (let* ((ansible-cnnx (or ansible-cnnx ansible-tramp-cnnx default-directory))
         (inventory-bin (or inventory-bin ansible-tramp-inventory-cli-exec))
         (ansible-inventory-cmd (concat inventory-bin " --list")))
    (deferred:$
      (deferred:next
        `(lambda ()
           (prf/tramp/remote-shell-command-to-string ,ansible-cnnx ,ansible-inventory-cmd)))
      (deferred:nextc it
        (lambda (x)
          (let ((json-object-type 'hash-table)
                (json-array-type 'list)
                (json-key-type 'string))
            (json-read-from-string x))))
      (deferred:nextc it
        callback))))

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


(defun ansible-tramp--exec-module-setup (host &optional ansible-cnnx ansible-bin ansible-user)
  "Launch Ansible shell command targeting HOST with module \"setup\", at remote path REMOTE-ANSIBLE-CNNX."
  (let ((ansible-cnnx (or ansible-cnnx ansible-tramp-cnnx default-directory))
        (ansible-cmd (ansible-tramp--build-ansible-cmd-setup host ansible-bin ansible-user)))
    (prf/tramp/remote-shell-command-to-string ansible-cnnx ansible-cmd)))


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

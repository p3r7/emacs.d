;;; vagrant-tramp-autoloads.el --- automatically extracted autoloads
;;
;;; Code:


;;;### (autoloads (vagrant-tramp-enable vagrant-tramp-term vagrant-tramp-ssh)
;;;;;;  "vagrant-tramp" "vagrant-tramp.el" (21798 15024))
;;; Generated autoloads from vagrant-tramp.el

(defconst vagrant-tramp-method "vagrant" "\
TRAMP method for vagrant boxes.")

(defvar vagrant-tramp-ssh (executable-find (concat (file-name-directory (or load-file-name buffer-file-name)) "bin/vagrant-tramp-ssh")) "\
The vagrant-tramp-ssh executable.")

(custom-autoload 'vagrant-tramp-ssh "vagrant-tramp" t)

(autoload 'vagrant-tramp-term "vagrant-tramp" "\
SSH to a Vagrant BOX in an `ansi-term'.

\(fn BOX)" t nil)

(autoload 'vagrant-tramp-enable "vagrant-tramp" "\
Add `vagrant-tramp-method' to `tramp-methods'.

\(fn)" nil nil)

;;;***

;;;### (autoloads nil nil ("vagrant-tramp-pkg.el") (21798 15024 658000))

;;;***

(provide 'vagrant-tramp-autoloads)
;; Local Variables:
;; version-control: never
;; no-byte-compile: t
;; no-update-autoloads: t
;; coding: utf-8
;; End:
;;; vagrant-tramp-autoloads.el ends here

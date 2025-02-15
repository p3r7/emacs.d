
(require 'url)



;; UPTSTREAM VERSIONS

;; On some versions of emacs, default TRAMP is buggy

(defvar prf/tramp/version-overide
  (cond
   ((and (= emacs-major-version 24)
         (= emacs-minor-version 3))
    "2.2.11")
   ((and (= emacs-major-version 24)
         (= emacs-minor-version 5))
    "2.2.12")))


(when (and prf/tramp/version-overide
           (executable-find "tar"))

  (let* ((tgz (concat "tramp-" prf/tramp/version-overide ".tar.gz"))
         (url (concat "https://ftp.gnu.org/gnu/tramp/" tgz))
         (localname (concat "~/.emacs.d/package-src/" tgz))
         unarchive-res)

    (mkdir "~/.emacs.d/package-src")
    (url-copy-file url localname)

    (setq unarchive-res
          (with-temp-buffer
            (save-excursion
              (cd "~/.emacs.d/package-src")
              ;; (process-file "tar" nil (current-buffer) nil "-xzf" localname)
              (shell-command (concat "tar -xzf " localname) (current-buffer)))))

    (unless (eq unarchive-res 0)
      (message "Attempted to install more recent version of TRAMP but failed to extract %s" localname))))


(let ((tramp-src (concat "~/.emacs.d/plugins-src/tramp-" prf/tramp/version-overide "/lisp")))
  (when (file-directory-p tramp-src)
    (add-to-list 'load-path (concat "~/.emacs.d/plugins-src/tramp-" prf/tramp/version-overide "/lisp"))))



;; MAIN

(use-package tramp
  :demand
  :config
  ;; (setq tramp-verbose 6)

  (add-to-list 'tramp-remote-path 'tramp-own-remote-path)

  ;; upstream value, takes into account recent ssh messages
  (setq tramp-yesno-prompt-regexp (concat
                                   (regexp-opt
                                    '("Are you sure you want to continue connecting (yes/no)?"
                                      "Are you sure you want to continue connecting (yes/no/[fingerprint])?")
                                    t)
                                   "\\s-*"))

  ;; disable vc for remote files (speed increase)
  (setq vc-ignore-dir-regexp
        (format "\\(%s\\)\\|\\(%s\\)"
                vc-ignore-dir-regexp
                tramp-file-name-regexp))

  (setq enable-remote-dir-locals t)

  ;; enable X11 forwarding
  ;; TODO: dynamically bind those w/ `with-shell-interpreter'
  ;; - local
  ;; (add-to-list 'tramp-remote-process-environment
  ;;              (format "DISPLAY=%s" (getenv "DISPLAY")))
  ;; - virtualbox
  ;; (add-to-list 'tramp-remote-process-environment
  ;;              (format "DISPLAY=localhost:10.0"))
  )

(use-package prf-tramp-method
  :quelpa (prf-tramp-method :fetcher github :repo "p3r7/prf-tramp")
  :after tramp)



;; SHELL & TERM

;; helpers, notably better remote shell commands

(use-package friendly-tramp-path
  :after tramp)

(use-package prf-tramp
  :quelpa (prf-tramp :fetcher github :repo "p3r7/prf-tramp")
  :after tramp)

;; ansible inventory
(use-package ansible-tramp
  :load-path "~/.emacs.d/plugins/ansible-tramp"
  :after (request-deferred friendly-remote-shell)
  :config
  (when ansible-tramp-inventory-http-url
    (ansible-tramp-set-inventory-cache-http)))

(use-package tramp-term
  :after (tramp)
  :defer t)



;; MORE METHODS

;; Docker
;; (with-eval-after-load 'tramp
;;   ;; NB: `featurep' doesn't seem to work in that case
;;   (if (locate-library (format "%s" 'tramp-container))
;;       (require 'tramp-container)
;;     (use-package docker-tramp)))

;; Vagrant
;;(when (and (executable-find "vagrant")
;;           (not (windows-nt-p)))
;;  (use-package vagrant-tramp
;;    :config
;;    (eval-after-load 'tramp
;;      '(vagrant-tramp-enable))))

;; KiTTY
(when (and (executable-find "kscp")
	       (executable-find "klink"))
  (use-package tramp-kitty
    :load-path "~/.emacs.d/plugins/tramp-kitty"
    :after (tramp)
    :config
    (tramp-kitty-set-session-map-cache)
    (with-eval-after-load 'putty-open
      (add-to-list 'putty-open-putty--session-methods "klinkx")
      (add-to-list 'putty-open-putty--ssh-methods "kscp")
      (add-to-list 'putty-open-putty--ssh-methods "klink"))))

;; SSH w/ key support
(use-package tramp-sshi
  :load-path "~/.emacs.d/plugins/tramp-sshi"
  :after (tramp))

;; PuTTY w/ SSH key support
(use-package tramp-plinki
  :load-path "~/.emacs.d/plugins/tramp-plinki"
  :after (tramp))

;; open in putty / kitty
(use-package putty-open
  :load-path "~/.emacs.d/plugins/putty-open"
  :after (tramp)
  :init
  (when (executable-find "kitty")
    (setq putty-open-putty-exec "kitty")))




(provide 'init-tramp)

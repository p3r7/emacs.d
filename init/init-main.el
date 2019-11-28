
(require 'dash)


 ;; DISABLED COMMANDS

(put 'erase-buffer 'disabled nil)
(put 'upcase-region 'disabled nil)
(put 'downcase-region 'disabled nil)
(put 'narrow-to-region 'disabled nil)

(global-set-key (kbd "C-c E") #'erase-buffer)



;; FILE FORMAT

(use-package files
  :ensure nil
  :custom
  (require-final-newline t)
  (enable-local-variables :all)
  (enable-local-eval t))

(setq-default indent-tabs-mode nil)
(setq tab-width 4)

(add-hook 'before-save-hook 'delete-trailing-whitespace)



;; BACKUPS

;; Backups
(setq
 ;;make-backup-files nil
 backup-directory-alist `(("." . ,prf-backup-dir))
 tramp-auto-save-directory prf-auto-save-dir ;; http://charles.plager.net/emacs.html
 password-cache-expiry nil
 ;;backup-by-copying t
 ;;backup-by-copying-when-linked t
 delete-old-versions t
 kept-new-versions 6
 kept-old-versions 2
 version-control t)



;; SECRETS / AUTH

;; Do not use gpg agent when runing in terminal
;; BUG: doesn't always work
(defadvice epg--start (around advice-epg-disable-agent activate)
  (let ((agent (getenv "GPG_AGENT_INFO")))
    (setenv "GPG_AGENT_INFO" nil)
    ad-do-it
    (setenv "GPG_AGENT_INFO" agent)))

(defvar prf/auth-sources
  (-filter #'file-exists-p '("~/.authinfo" "~/.netrc")))

(use-package auth-source
  :demand
  :no-require t
  :config
  (setq auth-sources prf/auth-sources))



;; LINE WRAP

;; TODO: this does not activate it globally
;; But do we really want this ? What are the use-cases ?
;; It breaks my `prf-smart-edit' under org
;; (visual-line-mode 1)




(provide 'init-main)

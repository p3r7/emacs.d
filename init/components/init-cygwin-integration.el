;; inspired by setup-cygwin package
;; not really suitable for a proper cywgin emacs install, more for using WinNT emacs alongside cygwin
;; TODO: also analyze this config http://stackoverflow.com/a/2078367
;; NOTE: might also need other tweaks http://zzamboni.org/blog/making-cygwin-windows-and-emacs-understand-th/



;; VARS

;; trick is that :
;; - cygwin's /usr/bin is linked to /bin, that is <cygwin-root>\bin\
;; - cygwin's /usr/local/bin is <cygwin-root>\usr\local\bin\

(setq cygwin-bin (concat cygwin-root "/bin")
      cygwin-local-bin (concat cygwin-root "/usr/local/bin"))

(setq prf/local-shell-bin/cygwin-bash (concat cygwin-bin "/bash.exe"))


 ;; EXEC-PATH ENRICHMENT

;; NB: cygwin's git misbehave w/ quelpa under Windows
;; thus we use git bash version by default instead, by putting it at the front of `exec-path'
;; (when (file-exists-p git-bash-cmd-root)
;;   (when (member git-bash-cmd-root exec-path)
;;     (setq exec-path (delete git-bash-cmd-root exec-path)))
;;   (setq exec-path (cons git-bash-cmd-root exec-path)))

;; likewise, quelpa only works well with MinGW's tar
;; (when (file-exists-p mingw-msys-bin-root)
;;   (when (member mingw-msys-bin-root exec-path)
;;     (setq exec-path (delete mingw-msys-bin-root exec-path)))
;;   (setq exec-path (cons mingw-msys-bin-root exec-path)))

(defun prf/add-cygwin-to-path ()
  "Enrich Emacs path w/ cygwin bin folders"
  (prf/enrich-exec-path cygwin-bin)
  (prf/enrich-exec-path cygwin-local-bin))

(prf/add-cygwin-to-path)



;; DOC

(defun prf/add-cygwin-info-dir ()
  "Add Cygwin docinfo dir to `Info-default-directory-list'"
  (setq Info-default-directory-list (append Info-default-directory-list (list cygwin-root))))

(prf/add-cygwin-info-dir)



;; SHELLS

;; TODO: use http://www.khngai.com/emacs/cygwin.php

;; (setq process-coding-system-alist '(("bash" . undecided-unix)))
;; (setq w32-quote-process-args ?\") ;; " @@@ IS THIS BETTER? ;@@@ WAS THIS BEFORE: (setq w32-quote-process-args t)

;; bash as default shell
;; (setq shell-file-name "C:/cygwin/bin/bash.exe") ; Subprocesses invoked via the shell.
;; (setenv "SHELL" shell-file-name)
;; (setq explicit-shell-file-name shell-file-name) ; Interactive shell
;; (setq ediff-shell shell-file-name)      ; Ediff shell
;; (setq explicit-shell-args '("--login" "-i"))
;;;;; (setq shell-command-switch "-ic") ; SHOULD THIS BE "-c" or "-ic"?

(when (executable-find prf/local-shell-bin/cygwin-bash)
  (with-eval-after-load 'prf-shell
    (defun prf/shell/cygwin-bash (&optional path)
      (interactive)
      ;; (prf/tramp/shell path prf/tramp/local-shell-bin/cygwin-bash)
      (prf-shell :path path :interpreter prf/local-shell-bin/cygwin-bash
                 :interpreter-args `("--init-file" ,(concat "/home/" (getenv "USERNAME") "/.bashrc"))))))



;; CODING STYLE

;;; Use Unix-style line endings.
;; (setq-default buffer-file-coding-system 'undecided-unix)



;; CYGWIN PTY COMPATIBILITY LAYER

(use-package fakecygpty
  :quelpa (fakecygpty :fetcher github :repo "d5884/fakecygpty")
  :if (executable-find "fakecygpty")
  :after (tramp prf-tramp-method)
  :config
  (fakecygpty-activate)

  (defun tramp-cywgin-ssh--get-enriched-tramp-methods ()
    (-map-when
     (lambda (e) (member (car e) '("ssh" "sshx")))
     (lambda (e) (prf/tramp/method-def/with-login-exec e "fakecygpty ssh"))
     tramp-methods))

  (setq tramp-methods (tramp-cywgin-ssh--get-enriched-tramp-methods)))



;; CYGPATH

;;; Handles old-style (text file) symlinks and new-style (.lnk file) symlinks.
;;; (Non-Cygwin-symlink .lnk files, such as desktop shortcuts, are still loaded as such.)
(defun follow-cygwin-symlink ()
  "Follow Cygwin symlinks.
Handles old-style (text file) and new-style (.lnk file) symlinks.
\(Non-Cygwin-symlink .lnk files, such as desktop shortcuts, are still
loaded as such.)"
  (save-excursion
    (goto-char 0)
    (if (looking-at
         "L\x000\x000\x000\x001\x014\x002\x000\x000\x000\x000\x000\x0C0\x000\x000\x000\x000\x000\x000\x046\x00C")
        (progn
          (re-search-forward
           "\x000\\([-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`][-A-Za-z0-9_\\.\\\\\\$%@(){}~!#^'`]+\\)")
          (find-alternate-file (match-string 1)))
      (if (looking-at "!<symlink>")
          (progn
            (re-search-forward "!<symlink>\\(.*\\)\0")
            (find-alternate-file (match-string 1))))
      )))

;;; Make Cygwin paths accessible
(use-package cygwin-mount
  :config
  (cygwin-mount-activate)
  (add-hook 'find-file-hooks 'follow-cygwin-symlink))




(provide 'init-cygwin-integration)

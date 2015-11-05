;; inspired by setup-cygwin package
;; not really suitable for a proper cywgin emacs install, more for using WinNT emacs alongside cygwin
;; TODO: also analyze this config http://stackoverflow.com/a/2078367
;; NOTE: might also need other tweaks http://zzamboni.org/blog/making-cygwin-windows-and-emacs-understand-th/

;; -------------------------------------------------------------------------

(defvar cygwin-root "c:/cygwin64")

(setq
 ;; cygwin-bin (concat cygwin-root "/usr/bin")
 cygwin-bin (concat cygwin-root "/bin")
 Info-default-directory-list (append Info-default-directory-list (list cygwin-root))
 )

;; trick is that :
;; - cygwin's /usr/bin is linked to /bin, that is <cygwin-root>\bin\
;; - cygwin's /usr/local/bin is <cygwin-root>\usr\local\bin\


;; -------------------------------------------------------------------------

;; exec-path enrichment

(setenv "PATH" (concat (prf/system/get-path-system-format cygwin-bin) ";" (getenv "PATH")))
(setq exec-path (cons cygwin-bin exec-path))


;; -------------------------------------------------------------------------

;; shells

;; (setq process-coding-system-alist '(("bash" . undecided-unix)))
;; (setq w32-quote-process-args ?\") ;; " @@@ IS THIS BETTER? ;@@@ WAS THIS BEFORE: (setq w32-quote-process-args t)

;; bash as default shell
;; (setq shell-file-name "C:/cygwin/bin/bash.exe") ; Subprocesses invoked via the shell.
;; (setenv "SHELL" shell-file-name)
;; (setq explicit-shell-file-name shell-file-name) ; Interactive shell
;; (setq ediff-shell shell-file-name)      ; Ediff shell
;; (setq explicit-shell-args '("--login" "-i"))
;;;;; (setq shell-command-switch "-ic") ; SHOULD THIS BE "-c" or "-ic"?


;; -------------------------------------------------------------------------

;; default coding style

;;; Use Unix-style line endings.
;; (setq-default buffer-file-coding-system 'undecided-unix)


;; -------------------------------------------------------------------------

;; make Emacs understand Cygwin-style path

;;; Make Cygwin paths accessible
(prf/require-plugin 'cygwin-mount)
(cygwin-mount-activate)


;; -------------------------------------------------------------------------

;; make Emacs follow Cygwin symlinks

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
(add-hook 'find-file-hooks 'follow-cygwin-symlink)

(provide 'init-cygwin-integration)

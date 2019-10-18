;; in emacs, Microsoft Windows is reffered to as either Win NT or w32
;; hard limit of 31 subprocesses to be spawned, see how to free old processes

(require 'prf-string)
(require 'dired)

;; ------------------------------------------------------------------------
;; DIRED

;; NB: `G' not taken into account for remote dirs but interpreted locally (on Windows at least)
;; this might be a bug of feature, due to either TRAMP or dired+
;; to see actual value for a dired buffer, look at `dired-actual-switches'
;; another, more generic, way to implement this would be to implement an action inspired by https://emacs.stackexchange.com/questions/35676/customize-direds-display and `dired-hide-details-mode'
(setq prf/dired-listing-switches "-alhG")


;; ------------------------------------------------------------------------
;; TRAMP

(when (executable-find "pscp")
  (setq tramp-default-method "pscp"))

;; ------------------------------------------------------------------------
;; FORMAT

;; DONE: defautl coding system tramp -> unix
;; http://edivad.wordpress.com/2007/04/03/emacs-convert-dos-to-unix-and-vice-versa/
;; http://www.masteringemacs.org/articles/2012/08/09/working-coding-systems-unicode-emacs/
;; http://stackoverflow.com/questions/2567600/m-characters-when-using-tramp-on-windows-to-connect-to-ubuntu-server
(setq default-buffer-file-coding-system 'utf-8-unix)
(prefer-coding-system 'utf-8-unix)

;; Hide ^M
(defun prf/hide-dos-eol ()
  "Do not show ^M in files containing mixed UNIX and DOS line endings."
  (interactive)
  (setq buffer-display-table (make-display-table))
  (aset buffer-display-table ?\^M []))
;; (add-hook 'text-mode-hook 'prf/hide-dos-eol)

(defvar prf/quelpa-tar-executable "C:/MinGW/msys/1.0/bin/tar.exe")

(when (file-exists-p prf/quelpa-tar-executable)
  (setq quelpa-build-tar-executable prf/quelpa-tar-executable))

;; ------------------------------------------------------------------------
;; TOOLS

(use-package w32-browser
  :config
  (defun prf/show-in-file-explorer ()
    (interactive)
    (let ((filename (if (equal major-mode 'dired-mode)
			default-directory
		      (buffer-file-name))))
      ;; (shell-command (concat "explorer.exe " (prf/system/get-path-system-format filename)))
      (w32explore (prf/system/get-path-system-format filename))))
  (defalias '_exp 'prf/show-in-file-explorer)
  (define-key dired-mode-map (kbd "Z") 'dired-w32-browser)
  (define-key dired-mode-map (kbd "E") 'prf/show-in-file-explorer))


(setq prf/setup/app/bash
      (executable-find "bash"))
;; (if (> (length prf/setup/app/bash) 0)
;; (progn
;; (setq shell-file-name "bash")
;; (setenv "SHELL" shell-file-name)
;; (setq explicit-shell-file-name shell-file-name)
;; )
;; )


;; ------------------------------------------------------------------------
;; CYGWIN



(if (executable-find "cygpath")
    (progn
      (setq cygwin-root (replace-regexp-in-string "/bin/cygpath.exe" "" (executable-find "cygpath")))
      (require 'init-cygwin-integration)))


;; ------------------------------------------------------------------------
;; KEYS

(setq w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil
      w32-pass-apps-to-system nil
      w32-lwindow-modifier 'super ; Left Windows key
      ;; w32-rwindow-modifier 'super ; Right Windows key
      ;; w32-apps-modifier 'hyper
      )

(w32-register-hot-key [s-])

;; ------------------------------------------------------------------------
;; SHELLS

(with-eval-after-load 'prf-tramp
  (setq prf/tramp/local-shell-bin/cmd shell-file-name)
  (defun prf/tramp/shell/cmd (&optional path)
    (interactive)
    (prf/tramp/shell path prf/tramp/local-shell-bin/cmd)))


;; ------------------------------------------------------------------------
;; WINDMOVE w/ AHK

;; function keys USED by Emacs (others can be used along AHK)
;; <f1>		help-command
;; <f10>	list-bookmarks
;; <f12>	my-theme-cycle
;; <f16>	clipboard-kill-ring-save
;; <f18>	clipboard-yank
;; <f2>		2C-command
;; <f20>	clipboard-kill-region
;; <f3>		kmacro-start-macro-or-insert-counter
;; <f4>		kmacro-end-or-call-macro
;; <f9>		deft

;; good to use: f15, f17, f19, f21

;; NOTE: moved this config inside host config, as very dependant on keyboard / Windows version

;; ------------------------------------------------------------------------
;; CONFIG FILES

;; AutoHotKey
(defun edit-dot-ahk () (interactive)
       (find-file (concat "c:/Users/" (getenv "USERNAME") "/Documents/AutoHotkey.ahk")))
;; (global-set-key (kbd "C-<f3>") 'edit-dot-ahk)



(defun edit-dot-dosrc () (interactive)
       (find-file (concat "c:/Users/" (getenv "USERNAME") "/Documents/dosrc.bat")))


;; ------------------------------------------------------------------------
;; FULLSCREEN

;; Fullscreen in windows
;; - method 1 using AHK (bound to f11)
;; - method 2, kinda crappy, has to be tweaked
;; (defun toggle-full-screen () (interactive) (shell-command "C:/Users/jordan.besly/AppData/Roaming/.emacs.d/emacs_fullscreen.exe"))
;; (global-set-key (kbd "C-<f11>") 'toggle-full-screen)
;; (defun toggle-full-screen-topmost () (interactive) (shell-command "C:/Users/jordan.besly/AppData/Roaming/.emacs.d/emacs_fullscreen.exe --topmost"))
;; (global-set-key (kbd "M-<f11>") 'toggle-full-screen-topmost)
;; - method  3, use toggle-frame-fullscreen from frame.el

;; ------------------------------------------------------------------------
;; EMACS DAEMON

;; Pseudo emacs-daemon behaviour
;;(require 'server-mode) ;; unecessary ?

;; (setq server-auth-dir (concat (getenv "APPDATA") "/.emacs.d/server"))
;; (make-directory server-auth-dir t)

(defadvice server-ensure-safe-dir
    (around my-around-server-ensure-safe-dir activate)
  "Ignores any errors raised from server-ensure-safe-dir"
  (ignore-errors ad-do-it))

(server-start)
;; (unless (server-running-p) (server-start)) ;; doesn't seem to work
(defun xy/done ()
  "Make Emacs frame invisible, just like the `emacs --daemon'"
  (interactive)
  (save-some-buffers) ;; Save edited buffers first!
  (server-edit)
  (make-frame-invisible nil t))
(when (equal system-type 'windows-nt)
  (global-set-key (kbd "C-x C-c") 'xy/done) ;; virtually kill a frame
  (global-set-key (kbd "C-x M-c") 'save-buffers-kill-emacs)) ;; kill emacs
(remove-hook 'kill-buffer-query-functions 'server-kill-buffer-query-function)


;; ------------------------------------------------------------------------
;; FONT

(setq
 prf/font "-outline-Consolas-normal-r-normal-normal-12-97-96-96-c-*-iso8859-1"
 inhibit-compacting-font-caches t)

;; ------------------------------------------------------------------------

;; CYGWIN

;;(prf/require-plugin 'setup-cygwin)

;; ------------------------------------------------------------------------

(provide 'init-w32)

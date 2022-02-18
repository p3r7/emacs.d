;; in emacs, Microsoft Windows is reffered to as either Win NT or w32
;; hard limit of 31 subprocesses to be spawned, see how to free old processes

(require 'prf-string)
(require 'dired)


;; DIRED

;; NB: `G' not taken into account for remote dirs but interpreted locally (on Windows at least)
;; this might be a bug of feature, due to either TRAMP or dired+
;; to see actual value for a dired buffer, look at `dired-actual-switches'
;; another, more generic, way to implement this would be to implement an action inspired by https://emacs.stackexchange.com/questions/35676/customize-direds-display and `dired-hide-details-mode'
(setq prf/dired-listing-switches "-alhG")



;; TRAMP

(when (executable-find "pscp")
  (setq tramp-default-method "pscp"))



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
;; (add-hook 'text-mode-hook #'prf/hide-dos-eol)

(defvar prf/quelpa-tar-executable "C:/MinGW/msys/1.0/bin/tar.exe")

(when (file-exists-p prf/quelpa-tar-executable)
  (setq quelpa-build-tar-executable prf/quelpa-tar-executable))



;; TOOLS

(use-package w32-browser
  :after dired
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


(defvar prf/setup/app/bash
  (executable-find "bash"))
;; (if (> (length prf/setup/app/bash) 0)
;; (progn
;; (setq shell-file-name "bash")
;; (setenv "SHELL" shell-file-name)
;; (setq explicit-shell-file-name shell-file-name)
;; )
;; )

(use-package w32shell
  :quelpa (w32shell :url "https://gist.githubusercontent.com/johnfredcee/1097145/raw/0b9995e00f7d239dd27bfd60b841760389ffd1b9/w32shell.el" :fetcher url))


;; ENV: CYGWIN

(setq cygwin-root nil)

(cond
 ((executable-find "cygpath")
  (setq cygwin-root (replace-regexp-in-string "/bin/cygpath.exe" "" (executable-find "cygpath"))))
 ((file-directory-p "C:/cygwin64")
  (setq cygwin-root "C:/cygwin64"))
 ((file-directory-p "C:/cygwin")
  (setq cygwin-root "C:/cygwin")))

(when cygwin-root
  (with-eval-after-load 'init-cygwin-integration
    (prf/add-cygwin-to-path)
    (prf/add-cygwin-info-dir)
    (defalias 'prf/shell/bash 'prf/shell/cygwin-bash))
  (require 'init-cygwin-integration))



;; ENV: MSYS

(require 'init-msys-integration)



;; ENV: GIT BASH

(require 'init-git-bash-integration)


 ;; KEYS

(setq w32-pass-lwindow-to-system nil
      ;; w32-pass-rwindow-to-system nil
      w32-pass-apps-to-system nil
      w32-lwindow-modifier 'super       ; Left Windows key
      ;; w32-rwindow-modifier 'super ; Right Windows key
      ;; w32-apps-modifier 'hyper
      )

(w32-register-hot-key [s-])

;; pass-through some practical commands
(w32-unregister-hot-key (kbd "<s-d>"))
(w32-unregister-hot-key (kbd "<s-r>"))
(w32-unregister-hot-key (kbd "<s-n>"))
(w32-unregister-hot-key (kbd "<s-t>"))
;; (w32-unregister-hot-key (kbd "<s-drag-mouse-1>"))


;; SHELLS

(with-eval-after-load 'friendly-shell
  (defun prf/shell/cmd (&optional path)
    (interactive)
    (when (and path
               (file-remote-p path))
      (setq path "~"))
    (friendly-shell :path path)))


;; CONFIG FILES

;; AutoHotKey
(defun edit-dot-ahk () (interactive)
       (find-file (concat "c:/Users/" (getenv "USERNAME") "/Documents/AutoHotkey.ahk")))

(defun edit-dot-dosrc () (interactive)
       (find-file (concat "c:/Users/" (getenv "USERNAME") "/Documents/dosrc.bat")))



;; FULLSCREEN

;; Fullscreen in windows
;; - method 1 using AHK (bound to f11)
;; - method 2, kinda crappy, has to be tweaked
;; (defun toggle-full-screen () (interactive) (shell-command "C:/Users/jordan.besly/AppData/Roaming/.emacs.d/emacs_fullscreen.exe"))
;; (global-set-key (kbd "C-<f11>") 'toggle-full-screen)
;; (defun toggle-full-screen-topmost () (interactive) (shell-command "C:/Users/jordan.besly/AppData/Roaming/.emacs.d/emacs_fullscreen.exe --topmost"))
;; (global-set-key (kbd "M-<f11>") 'toggle-full-screen-topmost)
;; - method  3, use toggle-frame-fullscreen from frame.el


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



;; DEFAULT FONT

(setq
 prf/font "-outline-Consolas-normal-r-normal-normal-12-97-96-96-c-*-iso8859-1"
 inhibit-compacting-font-caches t)




(provide 'init-w32)

;; in emacs, Microsoft Windows is reffered to as either Win NT or w32
;; hard limit of 31 subprocesses to be spawned, see how to free old processes

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


;; ------------------------------------------------------------------------
;; TOOLS

(setq prf/setup/app/bash
      (executable-find "bash"))
;; (if (> (length prf/setup/app/bash) 0)
;; (progn
;; (setq shell-file-name "bash")
;; (setenv "SHELL" shell-file-name)
;; (setq explicit-shell-file-name shell-file-name)
;; )
;; )


;; TODO: IntelliJ IDEA interractions
;; still WiP
(defun prf/open-in-intellij-idea (filepath &optional line)
  (let ((line (if line (int-to-string line) "0")))
    (shell-command (concat "\"C:\\Progra~2\\JetBrains\\IntelliJ IDEA 12.0.3\\bin\\idea.exe\" --line " line " " filepath)) )
  )


(defun prf/show-in-file-explorer ()
  (interactive)
  (shell-command (concat "explorer.exe " (get-path-system-format default-directory)) ))
(defalias '_exp 'prf/show-in-file-explorer)


;; ------------------------------------------------------------------------
;; CYGWIN



(if (executable-find "cygpath")
    (progn
      (setq cygwin-root (replace-regexp-in-string "bin/cygpath.exe" "" (executable-find "cygpath")))
      (require 'init-cygwin-integration)
      )
  )



;; ------------------------------------------------------------------------
;; KEYS

(setq w32-pass-lwindow-to-system nil
      w32-pass-rwindow-to-system nil
      w32-pass-apps-to-system nil
      w32-lwindow-modifier 'super ; Left Windows key
      w32-rwindow-modifier 'super ; Right Windows key
      ;; w32-apps-modifier 'hyper
      )


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

(setq
 key-windmove-left  "<C-f15>"
 key-windmove-righ  "<S-f15>"
 key-windmove-up    "<M-f15>"
 key-windmove-down  "<C-f17>"

 prf/key-split-window-vertically "<S-f17>"
 prf/key-split-window-horizontally "<M-f17>"
 prf/key-delete-other-windows "<C-f19>"
 prf/key-delete-window "<M-f19>"
 )


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
(defun toggle-full-screen () (interactive) (shell-command "C:/Users/jordan.besly/AppData/Roaming/.emacs.d/emacs_fullscreen.exe"))
(global-set-key (kbd "C-<f11>") 'toggle-full-screen)
(defun toggle-full-screen-topmost () (interactive) (shell-command "C:/Users/jordan.besly/AppData/Roaming/.emacs.d/emacs_fullscreen.exe --topmost"))
(global-set-key (kbd "M-<f11>") 'toggle-full-screen-topmost)


;; ------------------------------------------------------------------------
;; EMACS DAEMON

;; Pseudo emacs-daemon behaviour
;;(require 'server-mode) ;; unecessary ?
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
 )

;; ------------------------------------------------------------------------

;; CYGWIN

;;(prf/require-plugin 'setup-cygwin)

;; ------------------------------------------------------------------------

(provide 'init-w32)

;;; freedesktop-launch.el --- Parse and launch XDG desktop entries -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: processes, terminals
;; URL: https://github.com/p3r7/freedesktop-launch
;; Package-Requires: ((emacs "24.1")(cl-lib "0.6.1")(friendly-shell-command "0.2.0"))
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;  -----------
;;
;; For detailed instructions, please look at the README.md

;;; Code:



;; REQUIRES

(require 'cl-lib)
(require 'dash)
(require 's)

(require 'friendly-shell-command)



;; VARS

(defvar freedesktop-launch-default-data-dirs
  "/usr/local/share/:/usr/share/:/var/lib/snapd/desktop" "Fallback value for env var XDG_DATA_DIRS.")

(defvar freedesktop-launch-cnnx nil "Path to act from.
Only really usefull to set it for remote executions w/ TRAMP.
Support remote paths in form /<method>:<user>@<host>:/.")



;; COMMANDS

(defun freedesktop-launch-by-filename (data-dir entry &optional cnnx)
  "Launch .desktop ENTRY in DATA-DIR at location CNNX"
  (interactive)
  (setq data-dir (freedesktop-launch--sanitize-dir data-dir))
  (freedesktop-launch-by-filepath (data-dir "applications/" entry ".desktop") cnnx))


(defun freedesktop-launch-by-filepath (entry-file-path &optional cnnx)
  "Launch .desktop ENTRY in DATA-DIR at location CNNX"
  (interactive)
  (unless (freedesktop-launch--executable-find "dex" cnnx)
    (error "Command not found: dex"))
  (freedesktop-launch--launch-backround (list "dex" entry-file-path)))



;; ENTRIES

(defun freedesktop-launch-list-files (&optional cnnx)
  "Get alist of data dirs / .desktop entries at location CNNX"
  ;; get list of .desktop entries locations
  (let ((entries-locations (freedesktop-launch--data-dirs cnnx))
        (entries-locations (freedesktop-launch--data-dirs cnnx)))
    (--map (freedesktop-launch--list-apps-for-data-dir it cnnx) entries-locations)))

(defun freedesktop-launch-list-filepaths (&optional cnnx)
  "Get alist of data dirs / .desktop entries at location CNNX"
  ;; get list of .desktop entries locations
  (-flatten
   (--map
    (let ((data-dir (car it))
          (entries (cdr it)))
      (--map
       (freedesktop-launch--build-desktop-entry-path data-dir it)
       entries))
    (freedesktop-launch-list-files cnnx))))



;; PRIVATE HELPERS: EXECUTABLES

(defun freedesktop-launch--executable-find (command &optional cnnx)
  "Test COMMAND exists at location CNNX.
TRAMP-aware replacement for `executable-find'"
  (setq cnnx (or cnnx default-directory))
  (let ((default-directory cnnx))
    (freedesktop-launch--executable-find-dd command)))


(defun freedesktop-launch--executable-find-dd (command &optional _args)
  "Test COMMAND exists.
Drop in replacement for `executable-find' w/ support for remote
*nix servers.  The unused ARGS param makes it also a replacement
for `eshell-find-interpreter' which is slow with tramp
connections."
  (if (file-remote-p default-directory)
      (freedesktop-launch--executable-find-dd-cli command)
    (executable-find command)))


(defun freedesktop-launch--executable-find-dd-cli (command &optional _args)
  "Test COMMAND exists via cli.
Drop in replacement for `executable-find' for remote *nix
servers.  The unused ARGS param makes it also a replacement for
`eshell-find-interpreter' which is slow with tramp connections."
  (let (errno
        ;; (get-path-cmd (concat "which "command))
        (get-path-cmd (concat "command -v "command)))
    (with-temp-buffer
      (setq errno (shell-command get-path-cmd (current-buffer)))
      (when (eq errno 0)
        ;; NB: removing trailing \n
        (substring (buffer-string) 0 -1)))))



;; PRIVATE HELPERS: PROCESS

(defun freedesktop-launch--launch-backround (command)
  "Silently launch COMMAND in the background"
  (let ((kill-buffer-query-functions nil))
    (with-temp-buffer
      (make-process
       :name "tmp"
       ;; NB: :stderr is mandatory
       :stderr (current-buffer)
       :command '("dex" "/usr/local/share/applications/arduino-arduinoide.desktop"))
      ;; REVIEW: this is very hackish, there must be a better way, e.g.
      ;; killing the buffer via a process sentinel instead.

      ;; NB: `make-process' is async we need to force waiting for the
      ;; process to launch before current-buffer gets killed.
      (sleep-for 0 50))))



;; PRIVATE HELPERS: ENV VARS

(defun freedesktop-launch--getenv (variable &optional cnnx)
  "Get the value of environment variable VARIABLE, when at location CNNX.
Work even if CNNX is a remote path, given it's a *nix."
  (if (and cnnx
           (file-remote-p cnnx))
      (freedesktop-launch--getenv-cli variable cnnx)
    (getenv variable)))


(defun freedesktop-launch--getenv-cli (variable &optional cnnx)
  "Get the value of environment variable VARIABLE, when at location CNNX.
Work even if CNNX is a remote path, given it's a *nix."
  (let ((res
         (s-trim
          (friendly-shell-command-to-string (concat "echo $" variable)
                                            :path cnnx))))
    (unless (string= res "")
      res)))



;; PRIVATE HELPERS: DIRS

(defun freedesktop-launch--sanitize-dir (dir)
  "Ensure DIR ends with a \"/\""
  (concat (s-chop-suffix "/" dir) "/"))


(defun freedesktop-launch--build-dir-path-maybe-remote (dir &optional cnnx)
  "Build DIR path, prefixing it with Tramp prefix if CNNX is remote"
  (setq cnnx (or cnnx ""))
  ;; NB: when remote, `file-remote-p' returns prefix
  (concat (file-remote-p cnnx) dir))



;; PRIVATE HELPERS: FREEDESKTOP DATA DIRS


(defun freedesktop-launch--data-dirs (&optional cnnx)
  "Get list of Xdg data dirsat location CNNX"
  (let* ((env-data-dirs (freedesktop-launch--getenv "XDG_DATA_DIRS" cnnx))
         (env-data-dirs (or env-data-dirs freedesktop-launch-default-data-dirs))
         (entries-locations (split-string env-data-dirs ":"))
         (entries-locations (append entries-locations '("~/.local/share/")))
         (entries-locations (mapcar #'freedesktop-launch--sanitize-dir entries-locations)))
    entries-locations))


(defun freedesktop-launch--list-apps-for-data-dir (data-dir &optional cnnx)
  "Get list of .desktop entries in DATA-DIR at location CNNX"
  (let* ((app-dir (concat data-dir "applications"))
         (app-dir (freedesktop-launch--build-dir-path-maybe-remote app-dir cnnx)))
    (cons data-dir
          (when (file-directory-p app-dir)
            (--filter
             (string-match-p "\.desktop$" it)
             (directory-files app-dir))))))



;; PRIVATE HELPERS: FREEDESKTOP ENTRIES PARSING

(defun freedesktop-launch--build-desktop-entry-path (data-dir entry)
  (concat data-dir "applications/" entry))


(defun freedesktop-launch--parse-desktop-entry (data-dir entry &optional cnnx)
  "parse .desktop file ENTRY in DATA-DIR at location CNNX"
  (with-temp-buffer
    (insert-file-contents (freedesktop-launch--build-desktop-entry-path data-dir entry))
    (let ((lines (split-string (buffer-string) "\n" t))
          parsed)

      (cl-loop with current-section = nil
               with current-props = nil
               for l in lines

               ;; comment
               if (string-match "^#" l)
               do (progn)
               ;; section
               else if (string-match "^\\[\\(.*\\)\\]$" l)
               do
               (when current-section
                 (setq parsed (cons (cons current-section current-props) parsed))
                 (setq current-props nil))
               (setq current-section (match-string 1 l))
               ;; property
               else
               do
               (let* ((split (split-string l "="))
                      (k (car split))
                      (v (cdr split)))
                 (setq current-props (cons (cons k v) current-props)))
               finally do
               (setq parsed (cons (cons current-section current-props) parsed)))
      parsed)))




(provide 'freedesktop-launch)

;;; freedesktop-launch.el ends here

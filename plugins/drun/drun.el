;;; drun.el --- Parse and launch XDG desktop entries -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: processes, terminals
;; URL: https://github.com/p3r7/drun
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

(defvar drun-executable "dex" "Executable to run a desktop entry.")
(defvar drun-executable-opts '() "Command line options for `drun-executable'.")

(defvar drun-default-data-dirs
  "/usr/local/share/:/usr/share/:/var/lib/snapd/desktop" "Fallback value for env var XDG_DATA_DIRS.")

(defvar drun-cnnx nil "Path to act from.
Only really usefull to set it for remote executions w/ TRAMP.
Support remote paths in form /<method>:<user>@<host>:/.")



;; COMMANDS

(defun drun-by-filename (data-dir entry &optional cnnx)
  "Launch .desktop ENTRY in DATA-DIR at location CNNX"
  (interactive)
  (setq data-dir (drun--sanitize-dir data-dir))
  (drun-by-filepath (data-dir "applications/" entry ".desktop") cnnx))


(defun drun-by-filepath (entry-file-path &optional cnnx)
  "Launch .desktop ENTRY in DATA-DIR at location CNNX"
  (interactive)
  (unless (drun--executable-find drun-executable cnnx)
    (error (concat "Command not found: " drun-executable)))
  (drun--launch-backround (-flatten (list drun-executable drun-executable-opts entry-file-path))))



;; ENTRIES

(defun drun-list-files (&optional cnnx)
  "Get alist of data dirs / .desktop entries at location CNNX"
  ;; get list of .desktop entries locations
  (let ((entries-locations (drun--data-dirs cnnx))
        (entries-locations (drun--data-dirs cnnx)))
    (--map (drun--list-apps-for-data-dir it cnnx) entries-locations)))

(defun drun-list-filepaths (&optional cnnx)
  "Get alist of data dirs / .desktop entries at location CNNX"
  ;; get list of .desktop entries locations
  (-flatten
   (--map
    (let ((data-dir (car it))
          (entries (cdr it)))
      (--map
       (drun--build-desktop-entry-path data-dir it)
       entries))
    (drun-list-files cnnx))))



;; PRIVATE HELPERS: EXECUTABLES

(defun drun--executable-find (command &optional cnnx)
  "Test COMMAND exists at location CNNX.
TRAMP-aware replacement for `executable-find'"
  (setq cnnx (or cnnx default-directory))
  (let ((default-directory cnnx))
    (drun--executable-find-dd command)))


(defun drun--executable-find-dd (command &optional _args)
  "Test COMMAND exists.
Drop in replacement for `executable-find' w/ support for remote
*nix servers.  The unused ARGS param makes it also a replacement
for `eshell-find-interpreter' which is slow with tramp
connections."
  (if (file-remote-p default-directory)
      (drun--executable-find-dd-cli command)
    (executable-find command)))


(defun drun--executable-find-dd-cli (command &optional _args)
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

(defun drun--launch-backround (command)
  "Silently launch COMMAND in the background"
  (let ((kill-buffer-query-functions nil))
    (with-temp-buffer
      (make-process
       :name "tmp"
       ;; NB: :stderr is mandatory
       :stderr (current-buffer)
       :command command)
      ;; REVIEW: this is very hackish, there must be a better way, e.g.
      ;; killing the buffer via a process sentinel instead.

      ;; NB: `make-process' is async we need to force waiting for the
      ;; process to launch before current-buffer gets killed.
      (sleep-for 0 50))))



;; PRIVATE HELPERS: ENV VARS

(defun drun--getenv (variable &optional cnnx)
  "Get the value of environment variable VARIABLE, when at location CNNX.
Work even if CNNX is a remote path, given it's a *nix."
  (if (and cnnx
           (file-remote-p cnnx))
      (drun--getenv-cli variable cnnx)
    (getenv variable)))


(defun drun--getenv-cli (variable &optional cnnx)
  "Get the value of environment variable VARIABLE, when at location CNNX.
Work even if CNNX is a remote path, given it's a *nix."
  (let ((res
         (s-trim
          (friendly-shell-command-to-string (concat "echo $" variable)
                                            :path cnnx))))
    (unless (string= res "")
      res)))



;; PRIVATE HELPERS: DIRS

(defun drun--sanitize-dir (dir)
  "Ensure DIR ends with a \"/\""
  (concat (s-chop-suffix "/" dir) "/"))


(defun drun--build-dir-path-maybe-remote (dir &optional cnnx)
  "Build DIR path, prefixing it with Tramp prefix if CNNX is remote"
  (setq cnnx (or cnnx ""))
  ;; NB: when remote, `file-remote-p' returns prefix
  (concat (file-remote-p cnnx) dir))



;; PRIVATE HELPERS: FREEDESKTOP DATA DIRS


(defun drun--data-dirs (&optional cnnx)
  "Get list of Xdg data dirsat location CNNX"
  (let* ((env-data-dirs (drun--getenv "XDG_DATA_DIRS" cnnx))
         (env-data-dirs (or env-data-dirs drun-default-data-dirs))
         (entries-locations (split-string env-data-dirs ":"))
         (entries-locations (append entries-locations '("~/.local/share/")))
         (entries-locations (mapcar #'drun--sanitize-dir entries-locations)))
    entries-locations))


(defun drun--list-apps-for-data-dir (data-dir &optional cnnx)
  "Get list of .desktop entries in DATA-DIR at location CNNX"
  (let* ((app-dir (concat data-dir "applications"))
         (app-dir (drun--build-dir-path-maybe-remote app-dir cnnx)))
    (cons data-dir
          (when (file-directory-p app-dir)
            (--filter
             (string-match-p "\.desktop$" it)
             (directory-files app-dir))))))



;; PRIVATE HELPERS: FREEDESKTOP ENTRIES PARSING

(defun drun--build-desktop-entry-path (data-dir entry)
  (concat data-dir "applications/" entry))


(defun drun--parse-desktop-entry (data-dir entry &optional cnnx)
  "parse .desktop file ENTRY in DATA-DIR at location CNNX"
  (with-temp-buffer
    (insert-file-contents (drun--build-desktop-entry-path data-dir entry))
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




(provide 'drun)

;;; drun.el ends here

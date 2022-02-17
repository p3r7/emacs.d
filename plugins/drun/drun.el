;;; drun.el --- Parse and launch XDG desktop entries -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: processes, terminals
;; URL: https://github.com/p3r7/drun
;; Package-Requires: ((emacs "27.1")(cl-lib "0.6.1")(friendly-shell-command "0.2.0"))
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
(require 'async)

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
  (setq data-dir (drun--sanitize-dir data-dir))
  (setq cnnx (or cnnx default-directory))
  (drun-by-filepath (data-dir "applications/" entry ".desktop") cnnx))


(defun drun-by-filepath (entry-file-path &optional cnnx)
  "Launch .desktop ENTRY in DATA-DIR at location CNNX"
  (unless (drun--executable-find drun-executable cnnx)
    (error (concat "Command not found: " drun-executable)))
  (setq cnnx (or cnnx default-directory))
  (setq entry-file-path (drun--expand-file-name entry-file-path cnnx))
  (drun--launch-backround (-flatten (list drun-executable drun-executable-opts entry-file-path)) cnnx))



;; ENTRIES

(defun drun-list-files (&optional cnnx)
  "Get alist of data dirs / .desktop entries at location CNNX"
  ;; get list of .desktop entries locations
  (setq cnnx (or cnnx default-directory))
  (let ((entries-locations (drun--data-dirs cnnx)))
    (--map (drun--list-apps-for-data-dir it cnnx) entries-locations)))

(defun drun-list-filepaths (&optional cnnx)
  "Get alist of data dirs / .desktop entries at location CNNX"
  (drun-filter-duplicate-entries (drun-list-all-filepaths cnnx)))

(defun drun-list-all-filepaths (&optional cnnx)
  "Get alist of data dirs / .desktop entries at location CNNX, even duplicate ones."
  ;; get list of .desktop entries locations
  (-flatten
   (--map
    (let ((data-dir (car it))
          (entries (cdr it)))
      (--map
       (drun--build-desktop-entry-path data-dir it)
       entries))
    (drun-list-files cnnx))))

(defun drun-filter-duplicate-entries (entries)
  "Remove ENTRIES with same file name."
  (let ((loop-entries entries)
        entry duplicates)
    (while loop-entries
      (setq entry (car loop-entries))
      (setq duplicates (drun--get-duplicates-for-entry entry entries))

      (--each duplicates
        (setq entries (delete it entries))
        (setq loop-entries (delete it loop-entries)))

      (setq loop-entries (cdr loop-entries)))
    entries))

(defun drun--get-duplicates-for-entry (entry entries)
  "Get duplicates of ENTRY in ENTRIES."
  (let ((filename-entry (file-name-nondirectory entry)))
    (cdr (--filter
          (string= filename-entry (file-name-nondirectory it))
          entries))))



;; PRIVATE HELPERS: EXECUTABLES

(defun drun--executable-find (command &optional cnnx)
  "Test COMMAND exists at location CNNX.
TRAMP-aware replacement for `executable-find'"
  (setq cnnx (or cnnx default-directory))
  (let ((default-directory cnnx))
    (executable-find command 'honor-dd)))



;; PRIVATE HELPERS: PROCESS

(defun drun--start-process (name buffer program &rest program-args)
  "Fix of `start-process' that allows launching graphical applications.
The trick is to add the :stderr keyword arg to `make-process'."
  (unless (fboundp 'make-process)
    (error "Emacs was compiled without subprocess support"))
  (apply #'make-process
	 (append (list :name name
                       :stderr buffer   ; <- fix
                       :buffer buffer)
		 (if program
		     (list :command (cons program program-args))))))

(defun drun--start-file-process (name buffer program &rest program-args)
  "Fix of `start-file-process' that allows launching graphical applications.
See `drun--start-process' for more details."
  (let ((fh (find-file-name-handler default-directory 'start-file-process)))
    (if fh (apply fh 'start-file-process name buffer program program-args)
      (apply #'drun--start-process name buffer program program-args))))

(defun drun--tramp-handle-start-file-process (name buffer program &rest args)
  (tramp-file-name-handler
   'make-process
   :name name
   :buffer buffer
   :command (and program (cons program args))
   :noquery nil
   :file-handler t))

(defun drun--launch-backround-old (command)
  "Silently launch COMMAND in the background"
  (let ((kill-buffer-query-functions nil))
    (with-temp-buffer
      (make-process
       :name "tmp"
       ;; NB: :stderr is mandatory
       :stderr (current-buffer)
       :command command)

      ;; NB: `make-process' is async we need to force waiting for the
      ;; process to launch before current-buffer gets killed.
      ;; NB: this is hackish, but we can't use a sentinel as the buffer
      ;; keeps seeing a process as long as sub-process is running.
      (sleep-for 0 50))))



(defun drun--launch-backround (command &optional cnnx)
  "Silently launch COMMAND in the background"
  (setq cnnx (or cnnx default-directory))
  (let* ((default-directory cnnx)
         (buffer (generate-new-buffer "*drun*"))
         (process (apply #'drun--start-file-process
                         "drun"
                         buffer
                         command)))
    ))

(defun drun--launch-backround (command &optional cnnx)
  "Silently launch COMMAND in the background"
  ;; (setq cnnx (or cnnx default-directory))
  (setq cnnx "/ssh:vagrant@192.168.254.66:/vagrant/")
  (let ((command (s-join " " command)))
    (async-start
     (lambda ()
       (package-initialize)
       (require 'friendly-shell-command)
       (friendly-shell-command command
                               :path cnnx
                               :kill-buffer t)
       (sleep-for 10 50)
       command
       )
     (lambda (result)
       (message (concat "DONE: " result))
       )))
  )

(defun drun--launch-backround-bak (command &optional cnnx)
  "Silently launch COMMAND in the background"
  (setq cnnx (or cnnx default-directory))
  (let ((kill-buffer-query-functions nil))
    (with-temp-buffer
      (let* ((default-directory cnnx)
             (buffer (current-buffer))
             (process (apply #'drun--start-file-process
                             "drun"
                             buffer
                             command)))

        ;; NB: `make-process' is async we need to force waiting for the
        ;; process to launch before current-buffer gets killed.
        ;; NB: this is hackish, but we can't use a sentinel as the buffer
        ;; keeps seeing a process as long as sub-process is running.
        (sleep-for 0 50)))))


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


(defun drun--expand-file-name (name &optional cnnx)
  "Version of `expand-file-name' that also work on remote tramp CNNX."
  (setq cnnx (or cnnx default-directory))
  (if (and (file-remote-p cnnx)
           (s-starts-with? "~/" name))
      (let* ((vec (tramp-dissect-file-name cnnx))
             (user (tramp-file-name-user vec)))
        ;; FIXME: retrieve user home via env var or /etc/passwd
        (s-replace "~/" (concat "/home/" user "/") name))
    (expand-file-name name)))



;; PRIVATE HELPERS: FREEDESKTOP DATA DIRS

(defun drun--data-dirs (&optional cnnx)
  "Get list of Xdg data dirsat location CNNX"
  (let* ((env-data-dirs (drun--getenv "XDG_DATA_DIRS" cnnx))
         (env-data-dirs (or env-data-dirs drun-default-data-dirs))
         (entries-locations (split-string env-data-dirs ":"))
         (entries-locations (cons "~/.local/share/" entries-locations ))
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

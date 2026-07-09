;;; prf-quickrun-detect.el --- Generic content-based quickrun command detection  -*- lexical-binding: t; -*-

;; Author: prf
;; Description: ibuffer-inspired declarative rules for auto-detecting
;;   quickrun commands based on mode, filename, and buffer content.

;;; Commentary:

;; Define `prf/quickrun-custom-cmd-list' with entries that pair a quickrun
;; command definition with an ibuffer-style matcher.  When
;; `prf/quickrun-detect-custom-cmd' runs (typically from `find-file-hook'),
;; the first matching rule wins and sets `quickrun-option-cmdkey' buffer-locally.
;;
;; Supported filters in :match (implicitly AND'd at top level):
;;   (mode . MODE)           - `derived-mode-p' check (MODE or list of modes)
;;   (filename . REGEXP)     - match against `buffer-file-name'
;;   (content . (RE1 RE2 …)) - all regexps must be found in buffer
;;   (predicate . FORM)      - arbitrary elisp predicate
;;   (or FILTER1 FILTER2 …)  - any sub-filter matches
;;   (and FILTER1 FILTER2 …) - all sub-filters match (same as top-level)
;;   (not FILTER)            - negate a filter

;;; Code:

(require 'cl-lib)

(defvar prf/quickrun-custom-cmd-list nil
  "List of custom quickrun command rules.
Each entry is a plist with:

  :name        - (string) Command name registered with quickrun
  :command     - (string) Executable
  :exec        - (string or list) Exec template(s) (%%c = command, %%s = source)
  :description - (string, optional)
  :match       - ibuffer-style filter list, implicitly AND'd

See file commentary for the full list of supported filters.")

(defun prf/quickrun--match-filter (filter)
  "Evaluate a single FILTER against the current buffer.
Returns non-nil if the filter matches."
  (pcase filter
    (`(mode . ,(and modes (pred listp)))
     (apply #'derived-mode-p modes))
    (`(mode . ,mode)
     (derived-mode-p mode))
    (`(filename . ,regexp)
     (and buffer-file-name
          (string-match-p regexp buffer-file-name)))
    (`(content . ,regexps)
     (save-excursion
       (save-restriction
         (widen)
         (cl-every (lambda (re)
                     (goto-char (point-min))
                     (re-search-forward re nil t))
                   regexps))))
    (`(predicate . ,form)
     (eval form t))
    (`(or . ,filters)
     (cl-some #'prf/quickrun--match-filter filters))
    (`(and . ,filters)
     (cl-every #'prf/quickrun--match-filter filters))
    (`(not ,inner)
     (not (prf/quickrun--match-filter inner)))
    (_ (error "Unknown quickrun match filter: %S" filter))))

(defun prf/quickrun-detect-custom-cmd ()
  "Find first matching entry in `prf/quickrun-custom-cmd-list'.
Registers the command with quickrun and sets `quickrun-option-cmdkey'
buffer-locally when a match is found."
  (when (and (buffer-file-name)
             (bound-and-true-p prf/quickrun-custom-cmd-list))
    (cl-loop for entry in prf/quickrun-custom-cmd-list
             when (cl-every #'prf/quickrun--match-filter
                            (plist-get entry :match))
             do (let ((name (plist-get entry :name)))
                  (quickrun-add-command name
                    `((:command . ,(plist-get entry :command))
                      (:exec    . ,(plist-get entry :exec))
                      ,@(when (plist-get entry :description)
                          `((:description . ,(plist-get entry :description)))))
                    :mode major-mode)
                  (setq-local quickrun-option-cmdkey name))
             and return t)))

(defun prf/quickrun-resolve-command (&optional absolute-path)
  "Resolve the quickrun command for the current buffer.
Returns the full shell command string (or multiple commands joined
by \" && \" if chained).
When ABSOLUTE-PATH is non-nil (default for interactive use),
replace the bare source filename with its absolute path."
  (quickrun--set-executed-file)
  (let* ((src (or quickrun--executed-file (buffer-file-name)))
         (cmd-key (quickrun--command-key src))
         (cmd-info-hash (quickrun--fill-templates cmd-key src))
         (exec-cmds (gethash :exec cmd-info-hash))
         (joined (mapconcat #'identity exec-cmds " && ")))
    (if absolute-path
        (let* ((real-src (quickrun--real-file-name src))
               (basename (file-name-nondirectory real-src)))
          (replace-regexp-in-string (regexp-quote basename) real-src joined t t))
      joined)))

(defun prf/quickrun-copy-command ()
  "Copy the resolved quickrun command to the kill ring.
Uses absolute file paths so the command works from any directory."
  (interactive)
  (let ((cmd (prf/quickrun-resolve-command t)))
    (kill-new cmd)
    (message "Copied: %s" cmd)))

(provide 'prf-quickrun-detect)

;;; prf-quickrun-detect.el ends here

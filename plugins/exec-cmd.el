
(defvar prf/exec-cmd nil "file-local variable to override exec command")

(defun prf/exec-from-shebang ()
  "Parses the shebang (#!) for current buffer.
Inspired by src of `shell-script-mode'"
  (when (save-excursion
          (goto-char (point-min))
          (looking-at "#![ \t]?\\([^ \t\n]*/bin/env[ \t]\\)?\\([^ \t\n]+\\)"))
    (match-string 2)))

(defun prf/exec-cmd-eval (exec-cmd filename &optional shebang)
  (let ((dirname (file-name-directory filename)))
    ;; TODO: add parsing of #! and be able to refference it
    (s-replace-all `(("$FILEPATH" . ,filename)
                     ("$DIRNAME" . ,dirname)
                     ("$EXEC" . ,shebang))
                   exec-cmd)))

(defun prf/exec-based-on-filename (filename)
  "Determine the exec based on filename.
Inspired by src of `shell-script-mode'.
Modified to return nil instead of `sh-shell-file' as defautl value."
  (cond
   ((string-match "\\.m?spec\\'" filename) "rpm")
   ((string-match "[.]sh\\>"     filename) "sh")
   ((string-match "[.]bash\\>"   filename) "bash")
   ((string-match "[.]ksh\\>"    filename) "ksh")
   ((string-match "[.]mkshrc\\>" filename) "mksh")
   ((string-match "[.]t?csh\\(rc\\)?\\>" filename) "csh")
   ((string-match "[.]zsh\\(rc\\|env\\)?\\>" filename) "zsh")
   ((equal (file-name-nondirectory filename) ".profile") "sh")

   ;; not originally in `shell-script-mode'
   ((s-suffix? ".py" filename) "python") ;; REVIEW: should ideally test if in virtual env
   ((s-suffix? ".php" filename) "php")
   ((s-suffix? ".go" filename) "go run")))


(provide 'exec-cmd)

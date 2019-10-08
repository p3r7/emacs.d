

;; https://emacs.stackexchange.com/a/21478

;; (current-buffer)
;; (set-window-point (get-buffer-window "init.el") (point-max))
;; (with-current-buffer "init.el"
;;   (goto-char 42)
;;   (point))



;; MAJOR MODE

(defvar prf-multi-buffer-mode-map nil "Keymap for `prf-multi-buffer-mode'")

(define-derived-mode prf-multi-buffer-mode fundamental-mode "prf-multi-buffer"
  "major mode for editing multiple buffers at once."

  (setq prf-multi-buffer-mode-map (make-sparse-keymap))
  (define-key prf-multi-buffer-mode-map [t] 'prf-multi-buffer-mode--catch-all-key-fun)
  )


(defun prf-multi-buffer-mode--catch-all-key-fun ()
  (let ((key (vector last-input-event)))
    ;; (prf-multi-buffer-edit--get-assoc-key-fun)
    ))


;; FINDING KEY BINDING FOR BUFFER

;; TODO: see if `edmacro-format-keys' is not a better implementation
;; TODO: read about read-key vs
(defun prf-multi-buffer-edit--get-assoc-key-fun (buffer &optional key-sequence)
  "Inspired by `helpful-key'"
  (unless key-sequence
    ;; NB: read-key-sequence-vector might be better to work with
    (setq key-sequence (read-key-sequence "Press key: ")))
  (with-current-buffer buffer
    (let ((sym (key-binding key-sequence)))
      (when (commandp sym)
        sym))))

(defun prf-multi-buffer-edit--play-key (buffer &optional key-sequence)
  (unless key-sequence
    ;; NB: read-key-sequence-vector might be better to work with
    (setq key-sequence (read-key-sequence "Press key: ")))
  (with-current-buffer buffer
    (let ((cmd (prf-multi-buffer-edit--get-assoc-key-fun buffer key-sequence)))
      (when cmd
        (call-interactively cmd)))))

;; TODO: should first retrieve all commands, validate the execution, then play
;; indeed, some commands should be deduped, and prompted in some cases (e.g. `shell-command')
(defun prf-multi-buffer-edit--play-key-multi (buffer-list &optional key-sequence)
  (unless key-sequence
    ;; NB: read-key-sequence-vector might be better to work with
    (setq key-sequence (read-key-sequence "Press key: ")))
  (let ((predicate (lambda (x) (prf-multi-buffer-edit--play-key x key-sequence))))
    (mapc predicate buffer-list)))



;; INTERRACTION FUNCS

(defun prf-multi-buffer-edit--insert (buffer text)
  (with-current-buffer buffer
    (insert text)))

(defun prf-multi-buffer-edit--insert-all (buffer-list text)
  (let ((predicate (lambda (x) (prf-multi-buffer-edit--insert x text))))
    (mapc predicate buffer-list)))

(defun prf-multi-buffer-edit--forward-line (buffer &optional n)
  (with-current-buffer buffer
    (forward-line n)))

(defun prf-multi-buffer-edit--next-line (buffer &optional arg try-vscroll)
  (with-current-buffer buffer
    (next-line &optional arg try-vscroll)))



;; IDEA is to have something like ediff buffer to enter multi-buffer editing mode


;; https://emacs.stackexchange.com/questions/44093/edit-simultaneously-many-buffers-and-save-all-of-them-after-edit
;; https://nicolas.petton.fr/blog/mutli-occur-on-projects.html

;; https://github.com/magnars/multifiles.el

;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Two_002dColumn.html
;; https://emacs.stackexchange.com/questions/47200/copy-paste-text-among-split-window-buffers

;; catch all keys function mode map
;; https://stackoverflow.com/a/13215367


(provide 'prf-multi-buffer-edit)

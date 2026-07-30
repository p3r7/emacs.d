

;;; mwc.el --- Multiple Window Cursors -*- lexical-binding: t; -*-

;; Author: Jordan Besly
;; Version: 0.1.0
;; Package-Requires: ((emacs "27.1") (dash "2.19"))

;;; Commentary:

;; Multi-buffer editing mode inspired by ediff's control buffer approach.
;; Opens a control buffer at the bottom of the frame that broadcasts
;; keystrokes and cursor movement to all other visible windows.
;;
;; Usage:
;;   M-x mwc-start    - start editing current visible windows
;;   M-x mwc-start-grid - rearrange buffers in grid, then start
;;   C-c C-c or C-c C-q                 - quit multi-buffer editing
;;
;; References:
;; - https://emacs.stackexchange.com/questions/44093/edit-simultaneously-many-buffers-and-save-all-of-them-after-edit
;; - https://github.com/magnars/multifiles.el
;; - https://nicolas.petton.fr/blog/mutli-occur-on-projects.html

;;; Code:



;; deps

(require 'dash)
(require 'buffer-grid)



;; vars

(defgroup mwc nil
  "Edit multiple buffers simultaneously."
  :group 'editing)

(defvar mwc-max-columns 4
  "Maximum number of columns when displaying target buffers in a grid.")



;; internal state

(defvar-local mwc--target-windows nil
  "List of target windows for the current multi-buffer session.")
(put 'mwc--target-windows 'permanent-local t)

(defvar-local mwc--previous-window-config nil
  "Window configuration before multi-buffer mode was activated.")
(put 'mwc--previous-window-config 'permanent-local t)

(defvar-local mwc--region-overlays nil
  "Alist of (window . overlay) for region highlighting in target windows.")
(put 'mwc--region-overlays 'permanent-local t)

(defvar-local mwc--active nil
  "Non-nil when mwc-mode is active in this buffer.
Used as the toggle in `emulation-mode-map-alists' so that
mwc bindings override CUA and similar packages.")
(put 'mwc--active 'permanent-local t)



;; private helpers

(defun mwc--live-target-windows ()
  "Return the list of live target windows for the current session."
  (setq mwc--target-windows
        (--filter (window-live-p it) mwc--target-windows))
  mwc--target-windows)

(defun mwc--control-buffer-p (buf)
  "Return non-nil if BUF is a multi-buffer control buffer."
  (with-current-buffer buf
    (derived-mode-p 'mwc-mode)))

(defun mwc--unique-buffer-name (prefix suffix)
  "Construct a unique buffer name from PREFIX and SUFFIX."
  (let ((name (concat prefix suffix)))
    (if (null (get-buffer name))
        name
      (let ((n 2))
        (while (get-buffer (format "%s<%d>%s" prefix n suffix))
          (setq n (1+ n)))
        (format "%s<%d>%s" prefix n suffix)))))


;; region overlay management

(defun mwc--update-region-overlays ()
  "Update region highlight overlays in all target windows."
  (dolist (win (mwc--live-target-windows))
    (let* ((buf (window-buffer win))
           (ov (alist-get win mwc--region-overlays))
           (mark-pos (and (buffer-local-value 'mark-active buf)
                         (with-current-buffer buf (mark t)))))
      (if mark-pos
          (let* ((pt (window-point win))
                 (beg (min mark-pos pt))
                 (end (max mark-pos pt)))
            (if ov
                (move-overlay ov beg end buf)
              (setq ov (make-overlay beg end buf nil t))
              (overlay-put ov 'face 'region)
              (overlay-put ov 'mwc-region t)
              (setf (alist-get win mwc--region-overlays) ov)))
        (when ov
          (delete-overlay ov)
          (setf (alist-get win mwc--region-overlays) nil))))))

(defun mwc--cleanup-region-overlays ()
  "Remove all region highlight overlays."
  (dolist (entry mwc--region-overlays)
    (when (overlayp (cdr entry))
      (delete-overlay (cdr entry))))
  (setq mwc--region-overlays nil))

;; core logic: command propagation across windows

(defun mwc--exec-in-targets (cmd)
  "Execute CMD interactively in each live target window."
  (dolist (win (mwc--live-target-windows))
    (with-selected-window win
      (call-interactively cmd))))

(defmacro mwc--make-command (cmd)
  "Create an interactive lambda that executes CMD in all target windows."
  (let ((fn-name (intern (format "mwc--broadcast-%s" cmd))))
    `(progn
       (defun ,fn-name ()
         ,(format "Broadcast `%s' to all target windows." cmd)
         (interactive)
         (mwc--exec-in-targets #',cmd))
       #',fn-name)))

(defun mwc--self-insert ()
  "Insert the typed character in all target buffers at their point."
  (interactive)
  (let ((text (this-command-keys)))
    (dolist (win (mwc--live-target-windows))
      (with-selected-window win
        (insert text)))))

(defun mwc--toggle-mark ()
  "Toggle mark in all target windows, respecting CUA mode.
If the mark is active, deactivate it.  Otherwise, set it."
  (interactive)
  (dolist (win (mwc--live-target-windows))
    (with-selected-window win
      ;; Clear any stale deactivate-mark flag from a previous command;
      ;; the command loop normally does this but only for the current buffer.
      (setq deactivate-mark nil)
      (if (bound-and-true-p cua-mode)
          (call-interactively #'cua-set-mark)
        (call-interactively #'set-mark-command))
      ;; Process deactivate-mark immediately and clear the flag.
      (when deactivate-mark
        (deactivate-mark)
        (setq deactivate-mark nil)))))

(defun mwc--keyboard-quit ()
  "Deactivate mark in all target windows, then call `keyboard-quit'."
  (interactive)
  (dolist (win (mwc--live-target-windows))
    (with-selected-window win
      (deactivate-mark)))
  (keyboard-quit))



;; major mode

;; NB: we define broadcast commands before the keymap references them
(mwc--make-command left-char)
(mwc--make-command right-char)
(mwc--make-command forward-char)
(mwc--make-command backward-char)
(mwc--make-command previous-line)
(mwc--make-command next-line)
(mwc--make-command forward-word)
(mwc--make-command backward-word)
(mwc--make-command move-beginning-of-line)
(mwc--make-command move-end-of-line)
(mwc--make-command beginning-of-buffer)
(mwc--make-command end-of-buffer)
(mwc--make-command back-to-indentation)
(mwc--make-command forward-sentence)
(mwc--make-command backward-sentence)
(mwc--make-command forward-paragraph)
(mwc--make-command backward-paragraph)
(mwc--make-command scroll-up-command)
(mwc--make-command scroll-down-command)
(mwc--make-command delete-char)
(mwc--make-command delete-backward-char)
(mwc--make-command backward-delete-char-untabify)
(mwc--make-command kill-word)
(mwc--make-command backward-kill-word)
(mwc--make-command kill-line)
(mwc--make-command kill-whole-line)
(mwc--make-command yank)
(mwc--make-command yank-pop)
(mwc--make-command newline)
(mwc--make-command newline-and-indent)
(mwc--make-command open-line)
(mwc--make-command indent-for-tab-command)
(mwc--make-command undo)
(mwc--make-command set-mark-command)
(mwc--make-command exchange-point-and-mark)
(mwc--make-command kill-region)
(mwc--make-command copy-region-as-kill)

(defvar mwc-mode-map
  (let ((map (make-sparse-keymap)))
    ;; chars
    (define-key map [remap self-insert-command] #'mwc--self-insert)

    ;; cursor movement
    (define-key map [remap left-char] #'mwc--broadcast-left-char)
    (define-key map [remap right-char] #'mwc--broadcast-right-char)
    (define-key map [remap forward-char] #'mwc--broadcast-forward-char)
    (define-key map [remap backward-char] #'mwc--broadcast-backward-char)
    (define-key map [remap previous-line] #'mwc--broadcast-previous-line)
    (define-key map [remap next-line] #'mwc--broadcast-next-line)
    (define-key map [remap forward-word] #'mwc--broadcast-forward-word)
    (define-key map [remap backward-word] #'mwc--broadcast-backward-word)
    (define-key map [remap move-beginning-of-line] #'mwc--broadcast-move-beginning-of-line)
    (define-key map [remap move-end-of-line] #'mwc--broadcast-move-end-of-line)
    (define-key map [remap beginning-of-buffer] #'mwc--broadcast-beginning-of-buffer)
    (define-key map [remap end-of-buffer] #'mwc--broadcast-end-of-buffer)
    (define-key map [remap back-to-indentation] #'mwc--broadcast-back-to-indentation)
    (define-key map [remap forward-sentence] #'mwc--broadcast-forward-sentence)
    (define-key map [remap backward-sentence] #'mwc--broadcast-backward-sentence)
    (define-key map [remap forward-paragraph] #'mwc--broadcast-forward-paragraph)
    (define-key map [remap backward-paragraph] #'mwc--broadcast-backward-paragraph)
    (define-key map [remap scroll-up-command] #'mwc--broadcast-scroll-up-command)
    (define-key map [remap scroll-down-command] #'mwc--broadcast-scroll-down-command)

    ;; editing
    (define-key map [remap delete-char] #'mwc--broadcast-delete-char)
    (define-key map [remap delete-backward-char] #'mwc--broadcast-delete-backward-char)
    (define-key map [remap backward-delete-char-untabify] #'mwc--broadcast-backward-delete-char-untabify)
    (define-key map [remap kill-word] #'mwc--broadcast-kill-word)
    (define-key map [remap backward-kill-word] #'mwc--broadcast-backward-kill-word)
    (define-key map [remap kill-line] #'mwc--broadcast-kill-line)
    (define-key map [remap kill-whole-line] #'mwc--broadcast-kill-whole-line)
    (define-key map [remap yank] #'mwc--broadcast-yank)
    (define-key map [remap yank-pop] #'mwc--broadcast-yank-pop)
    (define-key map [remap newline] #'mwc--broadcast-newline)
    (define-key map [remap newline-and-indent] #'mwc--broadcast-newline-and-indent)
    (define-key map [remap open-line] #'mwc--broadcast-open-line)
    (define-key map [remap indent-for-tab-command] #'mwc--broadcast-indent-for-tab-command)
    (define-key map [remap undo] #'mwc--broadcast-undo)

    ;; mark / region
    (define-key map [remap set-mark-command] #'mwc--toggle-mark)
    (define-key map [remap exchange-point-and-mark] #'mwc--broadcast-exchange-point-and-mark)
    (define-key map [remap kill-region] #'mwc--broadcast-kill-region)
    (define-key map [remap copy-region-as-kill] #'mwc--broadcast-copy-region-as-kill)

    ;; quit / cancel
    (define-key map [remap keyboard-quit] #'mwc--keyboard-quit)
    (define-key map (kbd "C-c C-c") #'mwc-quit)
    (define-key map (kbd "C-c C-q") #'mwc-quit)

    map)
  "Keymap for `mwc-mode'.")

(define-derived-mode mwc-mode fundamental-mode "Multi-Edit"
  "Major mode for the multi-buffer edit control buffer.
All self-insert characters and cursor movement commands are
propagated to the target windows.

\\{mwc-mode-map}"
  (setq cursor-type 'box)
  (setq mwc--active t)
  (add-hook 'post-command-hook #'mwc--update-region-overlays nil t))



;; control buffer

(defun mwc--setup-control-buffer (control-buffer target-windows prev-config)
  "Set up CONTROL-BUFFER with TARGET-WINDOWS and PREV-CONFIG."
  (with-current-buffer control-buffer
    (mwc-mode)
    (setq mwc--target-windows target-windows)
    (setq mwc--previous-window-config prev-config)
    (erase-buffer)
    (insert (propertize "*** Multi-Buffer Edit ***" 'face 'bold))
    (insert "\n")
    (insert (format "Editing %d buffer(s): " (length target-windows)))
    (insert (mapconcat (lambda (w)
                         (propertize (buffer-name (window-buffer w))
                                     'face 'font-lock-string-face))
                       target-windows ", "))
    (insert "\n")
    (insert (propertize "C-c C-c" 'face 'font-lock-keyword-face)
            " quit")
    (insert " | Type here to broadcast to all buffers")
    (insert "\n\n")
    (set-buffer-modified-p nil)))



;; commands

;;;###autoload
(defun mwc-start ()
  "Start multi-buffer editing on all visible windows in the current frame.
Creates a control buffer at the bottom of the frame.  All keystrokes
typed in the control buffer are propagated to the target windows."
  (interactive)
  (let* ((prev-config (current-window-configuration))
         (target-windows (--filter
                          (not (mwc--control-buffer-p
                                (window-buffer it)))
                          (window-list nil 'no-minibuf)))
         (control-buffer-name
          (mwc--unique-buffer-name "*Multi-Edit " "*"))
         (control-buffer (get-buffer-create control-buffer-name)))

    (when (< (length target-windows) 1)
      (user-error "Need at least one visible buffer to edit"))

    (mwc--setup-control-buffer
     control-buffer target-windows prev-config)

    ;; Display control buffer in a side window at the bottom
    (let ((control-window (display-buffer-in-side-window
                           control-buffer
                           '((side . bottom)
                             (window-height . 5)
                             (dedicated . t)))))
      (select-window control-window)
      (goto-char (point-max)))))

;;;###autoload
(defun mwc-start-grid ()
  "Arrange visible buffers in a grid, then start multi-buffer editing.
Uses `buffer-grid-diplay' to lay out the target buffers before
activating the control buffer."
  (interactive)
  (let* ((prev-config (current-window-configuration))
         (buf-list (--filter
                    (not (mwc--control-buffer-p it))
                    (mapcar #'window-buffer
                            (window-list nil 'no-minibuf))))
         (control-buffer-name
          (mwc--unique-buffer-name "*Multi-Edit " "*"))
         (control-buffer (get-buffer-create control-buffer-name)))

    (when (< (length buf-list) 1)
      (user-error "Need at least one visible buffer to edit"))

    ;; Rearrange into a grid
    (delete-other-windows)
    (buffer-grid-diplay buf-list nil mwc-max-columns)

    ;; Collect resulting target windows (exclude the control buffer itself)
    (let ((target-windows (--filter
                           (not (eq (window-buffer it) control-buffer))
                           (window-list nil 'no-minibuf))))

      (mwc--setup-control-buffer
       control-buffer target-windows prev-config)

      ;; Display control buffer in a side window at the bottom
      (let ((control-window (display-buffer-in-side-window
                             control-buffer
                             '((side . bottom)
                               (window-height . 5)
                               (dedicated . t)))))
        (select-window control-window)
        (goto-char (point-max))))))

(defun mwc-quit ()
  "Quit multi-buffer editing and restore previous window configuration."
  (interactive)
  (let ((prev-config mwc--previous-window-config)
        (control-buffer (current-buffer)))
    (mwc--cleanup-region-overlays)
    (dolist (win (mwc--live-target-windows))
      (with-selected-window win
        (deactivate-mark)))
    (when (buffer-live-p control-buffer)
      (kill-buffer control-buffer))
    (when (window-configuration-p prev-config)
      (set-window-configuration prev-config))))




;; emulation-level keymap (overrides CUA and similar packages)

(defvar mwc--override-map
  (let ((map (make-sparse-keymap)))
    (define-key map (kbd "C-SPC") #'mwc--toggle-mark)
    map)
  "Keymap for mwc bindings that must override emulation-mode maps.\n
Registered in `emulation-mode-map-alists' so it takes priority over\nCUA's `cua-global-keymap' and similar high-priority keymaps.")

(defvar mwc--keymap-alist `((mwc--active . ,mwc--override-map)))
(add-to-list 'emulation-mode-map-alists 'mwc--keymap-alist)


(provide 'mwc)
;;; mwc.el ends here

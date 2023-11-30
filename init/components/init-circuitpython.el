
(with-eval-after-load 'vterm-toggle
  (defvar vterm-circuitpython-buff nil)

  (defvar circuitpython-tty "/dev/tty.usbmodem11101")

  (defun circuitpython-repl ()
    "Launch circuitpython repl.
To kill, as it's running under a screen session, use C-a k."
    (interactive)

    (unless (file-exists-p circuitpython-tty)
      (user-error ""))

    (let ((vterm-toggle-use-dedicated-buffer t)
          (vterm-toggle--vterm-dedicated-buffer vterm-circuitpython-buff))
      (with-current-buffer (vterm-toggle-cd)
        (setq vterm-circuitpython-buff (current-buffer))
        (rename-buffer "*circuitpython-repl*")
        (vterm-send-string (concat "screen " circuitpython-tty " 115200") t)
        (vterm-send-return)))))




(provide 'init-circuitpython)

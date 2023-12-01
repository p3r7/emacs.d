
;; NB: could theoretically not have to rely on `vterm' + `screen' using `make-serial-process', but it's kinda hard to make it behave properly

(with-eval-after-load 'vterm-toggle
  (defvar vterm-circuitpython-buff nil)

  (defun circuitpython-repl ()
    "Launch circuitpython repl.
To kill, as it's running under a screen session, use C-a k."
    (interactive)
    (let ((circuitpython-tty (car (f-glob "/dev/tty.usbmodem*")))
          (vterm-toggle-use-dedicated-buffer t)
          (vterm-toggle--vterm-dedicated-buffer vterm-circuitpython-buff))

      (unless (file-exists-p circuitpython-tty)
        (user-error "No circuitpython TTY found!"))

      (with-current-buffer (vterm-toggle-cd)
        (setq vterm-circuitpython-buff (current-buffer))
        (rename-buffer "*circuitpython-repl*")
        (vterm-send-string (concat "screen " circuitpython-tty " 115200") t)
        (vterm-send-return)))))




(provide 'init-circuitpython)

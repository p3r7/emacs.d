
(use-package aggressive-indent
  :hook
  (prog-mode . #'prog-mode-hook-aggressive-indent)
  :init
  ;; NB: `aggressive-indent-excluded-modes' exists but doesn't get bound untill calling `aggressive-indent-mode' once or using `global-aggressive-indent-mode'...
  (defvar prf/aggressive-indent-excluded-modes
    '(
      ;; `aggressive-indent-excluded-modes' defautl value
      elm-mode haskell-mode inf-ruby-mode makefile-mode makefile-gmake-mode python-mode sql-interactive-mode text-mode yaml-mode

      ;; added by me
      dockerfile-mode dotenv-mode sass-mode sql-mode
      ))
  (defun prog-mode-hook-aggressive-indent ()
    (unless (cl-member-if #'derived-mode-p prf/aggressive-indent-excluded-modes)
      aggressive-indent-mode)))




(provide 'init-aggressive-indent)


(require 'dash)


;; LINE NUMBERS

(defun prf/linum-prog-mode-hook ()
  (if (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode 1)
    (linum-mode 1)))

(add-hook 'prog-mode-hook #'prf/linum-prog-mode-hook)



;; LIGATURES

(when (fboundp 'prettify-symbols-mode)
  (setq prettify-symbols-unprettify-at-point 'right-edge))

(setq prf/ligature-mode-blacklist '(python-mode))

(defun prf/ligatures-prog-mode-hook ()
  (when (fboundp 'prettify-symbols-mode)
    (unless (derived-mode-p prf/ligature-mode-blacklist)
      (prettify-symbols-mode 1))))

(add-hook 'prog-mode-hook #'prf/ligatures-prog-mode-hook)




(provide 'init-prog-common)

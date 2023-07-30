
(require 'xref)




;; http://stackoverflow.com/questions/4643206/how-to-configure-indentation-in-emacs-lua-mode

(use-package lua-mode
  :mode "\\.p8$"
  :bind (("M-." . prf/lua/xref-find-definitions))

  :init
  (setq lua-indent-level 2)

  :config

  (defun prf/lua/xref-find-definitions (identifier)
    "Rework of `xref-find-definitions' that replacees generic `thing-at-point' w/ lua's variant (`lua-funcname-at-point')"
    (interactive (cl-letf (((symbol-function #'xref-backend-identifier-at-point) (lambda (_backend) (lua-funcname-at-point))))
                   (list (xref--read-identifier "Find definitions of: "))))
    (xref--find-definitions identifier nil))

  ;; PICO-8
  (defun p8-hook()
    (when (and (buffer-file-name)
	           (equal (file-name-extension (buffer-file-name)) "p8"))
      (setq-local lua-indent-level 1)))
  (add-hook 'lua-mode-hook #'p8-hook))



;; FENNEL

(use-package fennel-mode
  :mode "\\.fnl\\'")



;; PICO-8

(use-package pico8-mode
  :after (lua-mode)
  :quelpa (pico8-mode :fetcher github :repo "Kaali/pico8-mode"))



;; CUSTOM FNS - BYTES

(defun prf/hex-str-to-lua (str)
  (->> (s-split " " str)
       (--map (concat "0x" (s-downcase it)))
       (s-join ", ")))

(defun prf/lua/hex-to-table (start end)
  (interactive "r")
  (let ((lua-table (prf/hex-str-to-lua (buffer-substring start end))))
    (kill-region start end)
    (insert lua-table)))




(provide 'init-lua)

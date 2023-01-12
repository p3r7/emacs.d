


;; ENV

(defvar prf/supercolider-scel-path "~/.local/share/SuperCollider/Emacs/scel")

(let ((sclang-dir (concat prf/supercolider-scel-path "/el")))
  (when (file-directory-p sclang-dir)
    (normal-top-level-add-to-load-path (list sclang-dir))))



;; MODE(S)

(when (and (locate-library "sclang")
           (locate-library "sclang-vars"))
  (use-package sclang
    :ensure nil
    :demand)

  ;; (use-package sclang-extensions
  ;;   :after (sclang auto-complete)
  ;;   :hook ((sclang-mode . sclang-extensions-mode)))
  )




(provide 'init-supercollider)



(defvar prf/supercolider-scel-path "~/.local/share/SuperCollider/Emacs/scel")

(let ((sclang-dir (concat prf/supercolider-scel-path "/el")))
  (when (file-directory-p sclang-dir)
    (normal-top-level-add-to-load-path (list sclang-dir))))




(when (locate-library "sclang")
  (use-package sclang
    :ensure nil
    :demand))




(provide 'init-supercollider)

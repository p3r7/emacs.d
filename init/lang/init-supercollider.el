


;; ENV

(defvar prf/supercolider-scel-path "~/.local/share/SuperCollider/Emacs/scel")
(defvar prf/supercolider-bin "")

(when (darwin-p)
  (setq prf/supercolider-scel-path "~/Library/Application Support/SuperCollider/downloaded-quarks/scel")
  (setq prf/supercolider-bin "/Applications/SuperCollider.app/Contents/MacOS/")

  (when (file-directory-p prf/supercolider-bin)
    (setq exec-path (append exec-path (list prf/supercolider-bin)))))

(let ((sclang-dir (concat prf/supercolider-scel-path "/el")))
  (when (file-directory-p sclang-dir)
    (normal-top-level-add-to-load-path (list sclang-dir))))



;; MODE(S)

(when (locate-library "sclang")
  (use-package sclang
    :ensure nil
    :demand)

  ;; (use-package sclang-extensions
  ;;   :after (sclang auto-complete)
  ;;   :hook ((sclang-mode . sclang-extensions-mode)))
  )




(provide 'init-supercollider)

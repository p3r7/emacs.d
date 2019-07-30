
;; stolen from https://github.com/syl20bnr/spacemacs/tree/master/layers/%2Bthemes/theming

;; replaced `spacemacs--cur-theme' with `prf/theme/current-theme'

;; ------------------------------------------------------------------------
;; CONTEXT

(defvar theming-modifications '()
  "An alist of theme modifications. Each element should
be on the form (THEME . SPEC), where THEME is a symbol
representing a theme, and SPEC is an alist mapping faces
to face specs (see `defface').")

(defvar theming-headings-inherit-from-default '()
  "A list of themes where all headings should inherit
from the default face, or the symbol `all'.")

(defvar theming-headings-same-size '()
  "A list of themes where all headings should have the
same size, or the symbol `all'.")

(defvar theming-headings-bold '()
  "A list of themes where all headings should be bold,
or the symbol `all'.")

(defvar spacemacs--theming-header-faces
  '(font-latex-sectioning-0-face
    font-latex-sectioning-1-face
    font-latex-sectioning-2-face
    font-latex-sectioning-3-face
    font-latex-sectioning-4-face
    font-latex-sectioning-5-face
    font-latex-slide-title-face
    info-title-1
    info-title-2
    info-title-3
    info-title-4
    markdown-header-face
    markdown-header-face-1
    markdown-header-face-2
    markdown-header-face-3
    markdown-header-face-4
    markdown-header-face-5
    markdown-header-face-6
    org-document-title
    org-level-1
    org-level-2
    org-level-3
    org-level-4
    org-level-5
    org-level-6
    org-level-7
    org-level-8)
  "List of header faces.")


;; ------------------------------------------------------------------------
;; FUNCS

(defun spacemacs//in-or-all (key seq)
  (or (eq 'all seq) (memq key seq)))


(defun spacemacs//theming (theme &optional no-confirm no-enable)
  "Removes existing user theming and applies customizations for the given
theme."
  (unless no-enable

    ;; Headings
    ;; (let ((mods nil))
    ;;   (when (spacemacs//in-or-all theme theming-headings-inherit-from-default)
    ;;     (setq mods (plist-put mods :inherit 'default)))
    ;;   (when (spacemacs//in-or-all theme theming-headings-same-size)
    ;;     (setq mods (plist-put mods :height 1.0)))
    ;;   (when (spacemacs//in-or-all theme theming-headings-bold)
    ;;     (setq mods (plist-put mods :weight 'bold)))
    ;;   (when mods
    ;;     (dolist (face spacemacs--theming-header-faces)
    ;;       (custom-set-faces `(,face ((t ,mods)))))))

    ;; Add new modifications
    (dolist (spec (append (cdr (assq theme theming-modifications))
                          (cdr (assq t theming-modifications))))
      (custom-theme-set-faces theme `(,(car spec) ((t ,(cdr spec))))))))

(defun spacemacs/update-theme ()
  (interactive)
  (spacemacs//theming prf/theme/current-theme))


;; ------------------------------------------------------------------------
;; INIT

(defun theming/init-theming ()
  ;; Apply theme customizations after any call to load-theme
  (advice-add 'load-theme :after #'spacemacs//theming)
  ;; Apply the initial customizations now, because load-theme has already been called
  (spacemacs//theming prf/theme/current-theme))


;; ------------------------------------------------------------------------
;; END

(provide 'space-theming)

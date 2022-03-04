
(require 'dash)


;; LINE NUMBERS

(defun prf/linum-prog-mode-hook ()
  (if (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode 1)
    (linum-mode 1)))

(add-hook 'prog-mode-hook #'prf/linum-prog-mode-hook)



;; LIGATURES

(defvar prf/fav-ligature-backend 'prettify-symbols
  "Ligature system to use.

Either native \"prettify-symbols\" (works on all fonts) or \"ligature\" (uses the current font ligatures).")

(defun prf/ligature-minor-mode ()
  "Get ligature minor-mode according to `prf/fav-ligature-backend'."
  (cond
   ((eq prf/fav-ligature-backend 'ligature)
    #'ligature-mode)

   ((fboundp #'prettify-symbols-mode)
    #'prettify-symbols-mode)

   (:default
    nil)))

(when (>= emacs-major-version 28)
  (use-package ligature
    :quelpa (ligature :fetcher github :repo "mickeynp/ligature.el")
    :init
    (setq prf/fav-ligature-backend 'ligature)
    :config
    (ligature-set-ligatures 't '("www"))
    ;; (ligature-set-ligatures '(html-mode nxml-mode web-mode) '("<!--" "-->" "</>" "</" "/>" "://"))
    (ligature-set-ligatures 'prog-mode '("|||>" "<|||" "<==>" "<!--" "####" "~~>" "***" "||=" "||>"
                                         ":::" "::=" "=:=" "===" "==>" "=!=" "=>>" "=<<" "=/=" "!=="
                                         "!!." ">=>" ">>=" ">>>" ">>-" ">->" "->>" "-->" "---" "-<<"
                                         "<~~" "<~>" "<*>" "<||" "<|>" "<$>" "<==" "<=>" "<=<" "<->"
                                         "<--" "<-<" "<<=" "<<-" "<<<" "<+>" "</>" "###" "#_(" "..<"
                                         "..." "+++" "/==" "///" "_|_" "www" "&&" "^=" "~~" "~@" "~="
                                         "~>" "~-" "**" "*>" "*/" "||" "|}" "|]" "|=" "|>" "|-" "{|"
                                         "[|" "]#" "::" ":=" ":>" ":<" "$>" "==" "=>" "!=" "!!" ">:"
                                         ">=" ">>" ">-" "-~" "-|" "->" "--" "-<" "<~" "<*" "<|" "<:"
                                         "<$" "<=" "<>" "<-" "<<" "<+" "</" "#{" "#[" "#:" "#=" "#!"
                                         "##" "#(" "#?" "#_" "%%" ".=" ".-" ".." ".?" "+>" "++" "?:"
                                         "?=" "?." "??" ";;" "/*" "/=" "/>" "//" "__" "~~" "(*" "*)"
                                         "\\\\" "://"))))

(when (fboundp 'prettify-symbols-mode)
  (setq prettify-symbols-unprettify-at-point 'right-edge))

(setq prf/ligature-mode-blacklist '(python-mode))

(defun prf/ligatures-prog-mode-hook ()
  (when-let ((mode (prf/ligature-minor-mode)))
    (unless (derived-mode-p prf/ligature-mode-blacklist)
      (funcall mode 1))))

(add-hook 'prog-mode-hook #'prf/ligatures-prog-mode-hook)




(provide 'init-prog-common)

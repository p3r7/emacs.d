
(require 'dash)


;; LINE NUMBERS

(defun prf/linum-prog-mode-hook ()
  (if (fboundp 'display-line-numbers-mode)
      (display-line-numbers-mode 1)
    (linum-mode 1)))

(add-hook 'prog-mode-hook #'prf/linum-prog-mode-hook)



;; LIGATURES

(defvar prf/ligature/default-backend nil
  "Default ligature backedn (minor-mode) to use.")

(defvar prf/ligature/lang-backends
  '((emacs-lisp-mode . (prettify-symbols-mode
                        prf/ligature/ligature-mode))
    (clojure-mode . (prettify-symbols-mode))
    (python-mode . prf/ligature/ligature-mode))
  "Ligature backend (minor-mode) override for each major-mode.")

(defun prf/derived-mode-assq (alist &optional buff-mode)
  "Like `assq' but expects keys to be a major mode.
Tests that BUFF-MODE (opt., defaults to `major-mode')"
  (let ((buff-mode (or buff-mode major-mode)))
    (--some
     (and (provided-mode-derived-p buff-mode (car it))
          it)
     alist)))

(defun prf/derived-mode-alist-get (alist &optional buff-mode)
  "Like `alist-get' but expects keys to be a major mode.
Tests that BUFF-MODE (opt., defaults to `major-mode')"
  (cdr (prf/derived-mode-assq alist buff-mode)))

(defun prf/ligature/modes-for-current ()
  "Get ligature minor-modes for current buffer's majore-mode."
  (if-let* ((modes-w-override (-map #'car prf/ligature/lang-backends))
            (_ (apply #'derived-mode-p modes-w-override)))
      (prf/derived-mode-alist-get prf/ligature/lang-backends)
    (list prf/ligature/default-backend)))

(defvar prf/ligature/available-backends '() "List of available ligature backends")

(when (fboundp #'prettify-symbols-mode)
  (setq prettify-symbols-unprettify-at-point 'right-edge)

  (setq prf/ligature/default-backend #'prettify-symbols-mode)
  (add-to-list 'prf/ligature/available-backends #'prettify-symbols-mode))

(when (>= emacs-major-version 28)
  (use-package ligature
    :quelpa (ligature :fetcher github :repo "mickeynp/ligature.el")
    :init
    (setq prf/ligature/default-backend #'ligature-mode)
    (add-to-list 'prf/ligature/available-backends #'ligature-mode)
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
                                         "\\\\" "://"))
    (defvar prf/ligature-el/restricted-mode-ligatures
      '((emacs-lisp-mode . ("www" ";;" "<=" ">="
                            ;; dash
                            "->" "->>" "-->"
                            ))))

    (defun prf/ligature/ligature-mode (activate)
      "Custom wrapper around `ligature-mode' that allows ignoring global ligatures config, replacing it w/ one custom for current mode (from `prf/ligature-el/restricted-mode-ligatures')."
      (interactive)
      (if-let* ((custom-ligatures (prf/derived-mode-alist-get prf/ligature-el/restricted-mode-ligatures)))
          (let ((ligature-composition-table nil))
            (ligature-set-ligatures 't custom-ligatures)
            (ligature-mode activate))
        (ligature-mode activate)))))

(defun prf/ligatures-prog-mode-hook ()
  (when-let ((modes (prf/ligature/modes-for-current)))
    (--map
     (when (fboundp it)
       (funcall it 1))
     modes)))

(add-hook 'prog-mode-hook #'prf/ligatures-prog-mode-hook)




(provide 'init-prog-common)

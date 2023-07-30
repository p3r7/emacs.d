
;; NB: `dump-jump-go' & variants are deprecated
;; now have to use `xref-find-definitions' (`M-.') now

;; for rigrprep, install from deb in github repo
;; Debian/Ubunutu's version isn't compiled w/ the necessary PCRE2 flag
;; see https://bugs.debian.org/cgi-bin/bugreport.cgi?bug=926333
;;
;; can bes tested by calling rg w: the --pcre2 opt and getting:
;; "PCRE2 is not available in this build of ripgrep"

(use-package dumb-jump
  :init
  (setq dumb-jump-selector 'helm)
  (setq dumb-jump-force-searcher 'rg) ; to allow search ouside of project dir

  :config
  (add-hook 'xref-backend-functions #'dumb-jump-xref-activate)

  ;; NB: fix for TRAMP
  ;; patch from https://github.com/jacktasia/dumb-jump/pull/432/files
  (defun dumb-jump-fetch-results (cur-file proj-root lang _config &optional prompt)
    "Return a list of results based on current file context and calling grep/ag.
  CUR-FILE is the path of the current buffer.
  PROJ-ROOT is that file's root project directory.
  LANG is a string programming language with CONFIG a property list
  of project configuration."
    (let* ((cur-line-num (line-number-at-pos))
           (proj-config (dumb-jump-get-config proj-root))
           (config (when (s-ends-with? ".dumbjump" proj-config)
                     (dumb-jump-read-config proj-root proj-config)))
           (found-symbol (or prompt (dumb-jump-get-point-symbol)))
           (look-for (dumb-jump-process-symbol-by-lang lang found-symbol))
           (pt-ctx (if prompt
                       (get-text-property 0 :dumb-jump-ctx prompt)
                     (dumb-jump-get-point-context
                      (dumb-jump-get-point-line)
                      look-for
                      (dumb-jump--get-symbol-start))))
           (ctx-type
            (dumb-jump-get-ctx-type-by-language lang pt-ctx))

           (gen-funcs (dumb-jump-pick-grep-variant proj-root))
           (parse-fn (plist-get gen-funcs :parse))
           (generate-fn (plist-get gen-funcs :generate))
           (searcher (plist-get gen-funcs :searcher))

           (regexes (dumb-jump-get-contextual-regexes lang ctx-type searcher))

           (exclude-paths (when config (plist-get config :exclude)))
           (include-paths (when config (plist-get config :include)))
                                        ; we will search proj root and all include paths
           (search-paths (-distinct (-concat (list proj-root) include-paths)))
                                        ; run command for all
           (raw-results (--mapcat
                         ;; TODO: should only pass exclude paths to actual project root
                         (dumb-jump-run-command look-for it regexes lang exclude-paths cur-file
                                                cur-line-num parse-fn generate-fn)
                         search-paths))

           (tramp-path-prefix (or (file-remote-p default-directory) ""))

           (results (delete-dups
                     (--map
                      (progn
                        (plist-put it :target look-for)
                        (plist-put it :path
                                   (concat tramp-path-prefix (plist-get it :path))))
                      raw-results))))

      `(:results ,results :lang ,(if (null lang) "" lang) :symbol ,look-for :ctx-type ,(if (null ctx-type) "" ctx-type) :file ,cur-file :root ,proj-root)))

  )




(provide 'init-dumb-jump)

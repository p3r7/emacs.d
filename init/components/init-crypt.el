


;; GPG

(let ((gpg (executable-find "gpg2")))
  (when gpg
    (custom-set-variables `(epg-gpg-program ,gpg))
    (epa-file-enable)))




(provide 'init-crypt)

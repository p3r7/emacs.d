
(prf/require-plugin 'circe)

(setq circe-network-options
      `(("Freenode"
         :nick "p3r7"
         :channels ("#emacs" "#emacs-circe")
         :nickserv-password ,freenode-password)))

(provide 'init-circe)

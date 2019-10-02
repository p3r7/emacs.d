


;; LIB DEPENDENCIES

(use-package faces+
  :defer t
  :quelpa (faces+ :fetcher github :repo "emacsmirror/faces-plus"))

(use-package frame-fns
  :defer t
  :quelpa (frame-fns :fetcher github :repo "emacsmirror/frame-fns"))

(use-package hexrgb
  :defer t
  :quelpa (hexrgb :fetcher github :repo "emacsmirror/hexrgb"))



;; MAIN

(use-package doremi
  :defer t
  :quelpa (doremi :fetcher github :repo "emacsmirror/doremi"))

(use-package doremi-mac
  :defer t
  :quelpa (doremi-mac :fetcher github :repo "emacsmirror/doremi-mac")
  :after (doremi))

(use-package doremi-frm
  :defer t
  :quelpa (doremi-frm :fetcher github :repo "emacsmirror/doremi-frm")
  :after (doremi faces+ frame-fns hexrgb))

(use-package doremi-cmd
  :defer t
  :quelpa (doremi-cmd :fetcher github :repo "emacsmirror/doremi-cmd")
  :after (doremi))




(provide 'init-doremi)

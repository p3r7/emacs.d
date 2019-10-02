


;; LIB DEPENDENCIES

(use-package faces+
  :quelpa (faces+ :fetcher github :repo "emacsmirror/faces-plus"))

(use-package frame-fns
  :quelpa (frame-fns :fetcher github :repo "emacsmirror/frame-fns"))



;; MAIN

(use-package doremi
  :quelpa (doremi :fetcher github :repo "emacsmirror/doremi"))

(use-package doremi-mac
  :quelpa (doremi-mac :fetcher github :repo "emacsmirror/doremi-mac")
  :after (doremi))

(use-package doremi-frm
  :quelpa (doremi-frm :fetcher github :repo "emacsmirror/doremi-frm")
  :after (doremi faces+ frame-fns))

(use-package doremi-cmd
  :quelpa (doremi-cmd :fetcher github :repo "emacsmirror/doremi-cmd")
  :after (doremi))


(provide 'init-doremi)

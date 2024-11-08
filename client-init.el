;; stuff to intrepret for each launch of emacs client
;; mainly to correct bugs induced by winNT version of emacs

;; -e doesn't appear to work, need to use --eval w/ a load-file http://stackoverflow.com/questions/2803037/emacsclient-eval-insert-something-is-not-working-for-me

(mapc
 (lambda (frame)
   (with-selected-frame frame
     (space-theming-update-current-theme)))
 (frame-list))


;; (message "frame-list")
;; (message "%S" (frame-list))

;; (message "client-init-current-frame")
;; (message "%S" (selected-frame))


;; (prf/customize-helm-tty-faces-maybe)
;; (call-interactively #'prf/scratch)
;; (prf/tty-setup-frame-hook)

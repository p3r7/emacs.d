


;; DEPS

(require 'ts)

(require 'pushbullet-api)


;; META-DATA

(defgroup pushbullet nil
  "Mode for interracting with pushbullet."
  :group 'external)



;; MAJOR MODE

(define-derived-mode pushbullet-mode tabulated-list-mode "Pushbullet pushes"
  "Major mode for interracting with Pushbullet"
  :group 'pushbullet

  (setq tabulated-list-format [("At" 25 t)("Label" 100 t)("URL" 50 t)("From" 30 t)])
  (setq tabulated-list-sort-key '("At" . nil))
  (pushbullet-refresh-sync)
  (unless (cdr tabulated-list-sort-key)
    ;; Invert initial sort order, putting most recent items on top
    (tabulated-list-sort 0))  )



;; COMMANDS

(defun pushbullet ()
  "Show Pushbullet interactive buffer."
  (interactive)
  (switch-to-buffer (get-buffer-create "*pushbullet*"))
  (pushbullet-mode))

(defun pushbullet-refresh-sync ()
  "Refresh list of pushes."
  (interactive)

  (let* ((all-pushes (pushbullet-api-get-all-pushes-sync :limit 100))
         (normalized-pushes (mapcar #'push-normalize all-pushes)))
    (setq tabulated-list-entries normalized-pushes)))

(defun push--label-key (type)
  (cond ((string= "note" type) 'body)
        ((string= "link" type) 'title)
        (t (error "Unexpected type %s for push" type))))

(defun push--url-key (type)
  (cond ((string= "note" type) nil)
        ((string= "link" type) 'url)
        (t (error "Unexpected type %s for push" type))))

(defun push-normalize (push)
  (let* ((id (alist-get 'iden push))
         (type (alist-get 'type push))
         (label-k (push--label-key type))
         (label (concat (alist-get label-k push)))
         (url-k (push--url-key type))
         (url (concat (alist-get url-k push)))
         (pushed-at-unix-ts (alist-get 'created push))
         (pushed-at-ts (make-ts :unix pushed-at-unix-ts))
         (pushed-at (ts-format pushed-at-ts))
         (is-from-me (string= "self" (alist-get 'direction push)))
         (from (if is-from-me
                   "myself"
                 (alist-get 'sender_email_normalized push))))
    (list id
          (vector pushed-at label url from))))



(provide 'pushbullet)

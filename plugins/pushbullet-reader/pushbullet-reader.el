

;; DEPS

(require 'cl-lib)
(require 'dash)
(require 'kv)

(require 'json)
(require 'request)



;; VARS

(defvar pushbullet-reader-access-token nil "API token to generate at <https://www.pushbullet.com/#settings>")



;; API

(cl-defun pushbullet-reader--get (endpoint &key params sync)
  "Return response struct for an API request to <https://api.pushbullet.com/v2/ENPOINT>.

ENDPOINT may be a string or symbol, e.g. `users/me'.  DATA should be a
plist of API parameters; keys with nil values are removed.  SYNC
is passed to `request''s `:sync' keyword.

The response body is automatically parsed with `json-read'."
  (declare (indent defun))

  (unless pushbullet-reader-access-token
    (error "Empty API Access Token in var `pushbullet-reader-access-token'"))

  (let* ((endpoint (cl-typecase endpoint
                     (symbol (symbol-name endpoint))
                     (string endpoint)))
         ;; (request-backend 'url-retrieve)
         (url (concat "https://api.pushbullet.com/v2/" endpoint))
         (params (kvplist->alist (pocket-lib--plist-non-nil params)))
         (json-array-type 'list))
    (request url
      :type "GET"
      :headers `(("Access-Token" . ,pushbullet-reader-access-token))
      :params params
      :sync sync
      :parser #'json-read
      :success (cl-function
                (lambda (&key data &allow-other-keys)
                  data))
      :error (cl-function
              (lambda (&key data error-thrown symbol-status response &allow-other-keys)
                (error "Request error: URL:%s  DATA:%s  ERROR-THROWN:%s  SYMBOL-STATUS:%s  RESPONSE:%s"
                       url data error-thrown symbol-status response))))))


(cl-defun pushbullet-reader-get-pushes (&key
                                        (limit 10)
                                        (active "true")
                                        modified_after
                                        cursor)
  "Return JSON response for a \"pushes\" API request.
Without any arguments, this simply returns the first 10
undeleted items in the user's list.  Keys set to nil will
not be sent in the request.  See
<https://docs.pushbullet.com/#list-pushes>."
  (declare (indent defun))
  (let ((limit (number-to-string limit))
        (params (list :limit limit :cursor cursor
                      :active active
                      :modified_after modified_after)))
    (request-response-data
     (pushbullet-reader--get 'pushes
       :params params
       :sync t))))

(cl-defun pushbullet-reader-get-all-pushes (&key
                                            (active "true")
                                            modified_after
                                            cursor)
  ;; TODO: make it async using `deferred' or `aio'
  (cl-loop
   until (string= cursor "END")
   ;; repeat 500
   append
   (let* ((res (pushbullet-reader-get-pushes :cursor cursor))
          (next-cursor (cdr (assoc 'cursor res))))
     (setq cursor next-cursor)
     (unless cursor
       (setq cursor "END"))
     (cdr (assoc 'pushes res)))))


(defun pushbullet-reader-get-push-text (push)
  (cond
   ((string= (cdr (assoc 'type push)) "note")
    (cdr (assoc 'body push)))
   ((string= (cdr (assoc 'type push)) "link")
    (cdr (assoc 'url push)))))


;; HELPERS

(defun pushbullet-reader--plist-non-nil (plist)
  "Return PLIST without key-value pairs whose value is nil."
  (cl-loop for (key value) on plist by #'cddr
           unless (null value)
           append (list key value)))




(provide 'pushbullet-reader)

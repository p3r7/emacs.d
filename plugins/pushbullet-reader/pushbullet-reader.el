

;; DEPS

(require 'cl-lib)
(require 'dash)
(require 'kv)

(require 'json)
(require 'request)



;; VARS

(defvar pushbullet-reader-access-token nil "API token to generate at <https://www.pushbullet.com/#settings>")



;; API

(cl-defun pushbullet-reader--get (endpoint &key params callback)
  "Return response struct for an API request to <https://api.pushbullet.com/v2/ENPOINT>.

ENDPOINT may be a string or symbol, e.g. `users/me'.  DATA should be a
plist of API parameters; keys with nil values are removed.

Call is synchronous unless CALLBACK is passed as an argument.

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

    (if callback
        (deferred:$
          (request-deferred url
                            :type "GET"
                            :headers `(("Access-Token" . ,pushbullet-reader-access-token))
                            :params params
                            :parser
                            (lambda ()
                              (let ((json-array-type 'list))
                                (json-read))))
          (deferred:nextc it
            callback))
      ;; (request url
      ;;   :type "GET"
      ;;   :headers `(("Access-Token" . ,pushbullet-reader-access-token))
      ;;   :params params
      ;;   :sync nil
      ;;   :parser #'json-read
      ;;   :success (cl-function
      ;;             (lambda (&key data &allow-other-keys)
      ;;               (setq prf/toto3 (request-response-data data))))
      ;;   :error (cl-function
      ;;           (lambda (&key data error-thrown symbol-status response &allow-other-keys)
      ;;             (error "Request error: URL:%s  DATA:%s  ERROR-THROWN:%s  SYMBOL-STATUS:%s  RESPONSE:%s"
      ;;                    url data error-thrown symbol-status response))))
      (request url
        :type "GET"
        :headers `(("Access-Token" . ,pushbullet-reader-access-token))
        :params params
        :sync 't
        :parser #'json-read
        :success (cl-function
                  (lambda (&key data &allow-other-keys)
                    data))
        :error (cl-function
                (lambda (&key data error-thrown symbol-status response &allow-other-keys)
                  (error "Request error: URL:%s  DATA:%s  ERROR-THROWN:%s  SYMBOL-STATUS:%s  RESPONSE:%s"
                         url data error-thrown symbol-status response)))))))


(cl-defun pushbullet-reader-get-pushes (&key
                                        (limit 20)
                                        (active "true")
                                        modified_after
                                        cursor
                                        callback)
  "Return JSON response for a \"pushes\" API request.
Without any arguments, this simply returns the first 10
undeleted items in the user's list.  Keys set to nil will
not be sent in the request.  See
<https://docs.pushbullet.com/#list-pushes>."
  (declare (indent defun))
  (let ((limit (when limit (number-to-string limit)))
        (params (list :limit limit :cursor cursor
                      :active active
                      :modified_after modified_after)))
    (if callback
        (pushbullet-reader--get 'pushes
          :params params
          :callback callback)
      (request-response-data
       (pushbullet-reader--get 'pushes
         :params params
         :callback callback)))))

(cl-defun pushbullet-reader-get-all-pushes-async (&key
                                                  limit
                                                  (active "true")
                                                  modified_after
                                                  cursor
                                                  callback
                                                  final-callback
                                                  (all-pushes '()))
  (pushbullet-reader-get-pushes
    :limit limit                        ; NB: limiting unitary calls but no effect unless <= 20
    :active active
    :modified_after modified_after
    :cursor cursor
    :callback
    `(lambda (response)
       (when (request-response-error-thrown response)
         (error "Got request-deferred error, aborting"))
       (when ,callback
         (funcall ,callback response))
       (let* ((res (request-response-data response))
              (next-cursor (cdr (assoc 'cursor res)))
              (pushes (cdr (assoc 'pushes res)))
              (limit ,limit))
         (unless (null limit)
           (setq limit (- limit (length pushes))))
         (setq pushes (append pushes ',all-pushes))
         (if (and next-cursor
                  (> limit 0))
             (progn
               (pushbullet-reader-get-all-pushes-async :limit limit
                                                       :active ,active
                                                       :modified_after ,modified_after
                                                       :cursor next-cursor
                                                       :callback ,callback
                                                       :final-callback ,final-callback
                                                       :all-pushes pushes))
           (when ,final-callback
             (funcall ,final-callback pushes)))))))

(cl-defun pushbullet-reader-get-all-pushes-sync (&key
                                                 (active "true")
                                                 modified_after
                                                 cursor)
  (cl-loop
   until (string= cursor "END")
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

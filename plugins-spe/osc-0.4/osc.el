;;; osc.el --- Open Sound Control protocol library  -*- lexical-binding:t -*-

;; Copyright (C) 2014-2021  Free Software Foundation, Inc.

;; Author: Mario Lang <mlang@blind.guru>
;; Version: 0.4
;; Keywords: comm, processes, multimedia

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; OpenSound Control ("OSC") is a protocol for communication among
;; computers, sound synthesizers, and other multimedia devices that is
;; optimized for modern networking technology and has been used in many
;; application areas.

;; This package implements low-level functionality for OSC clients and servers.
;; In particular:
;; * `osc-make-client' and `osc-make-server' can be used to create process objects.
;; * `osc-send-message' encodes and sends OSC messages from a client process.
;; * `osc-server-set-handler' can be used to change handlers for particular
;;   OSC paths on a server process object on the fly.

;; Usage:
;;
;; Client: (setq my-client (osc-make-client "127.0.0.1" 7770))
;;         (osc-send-message my-client "/osc/path" 1.5 1.0 5 "done")
;;         (delete-process my-client)
;;
;; Server: (setq my-server (osc-make-server "127.0.0.1" 7770
;;          (lambda (path &rest args)
;;            (message "OSC %s: %S" path args))))

;;; Code:

(require 'cl-lib)

(defun osc-string (string)
  (setq string (encode-coding-string string 'binary))
  (concat string (make-string (1+ (- 3 (% (length string) 4))) 0)))

(defun osc-blob (vector)
  (let ((length (length vector)))
    (concat (osc-int32 length)
	    (apply #'unibyte-string (append vector nil))
	    (make-string (% (- 4 (% length 4)) 4) 0))))

(defun osc-float32 (value)
  (let (s (e 0) f)
    (cond
     ((= value 0.0)
      (setq s (if (< (copysign 1.0 value) 0) 1 0) f 0))
     ((= value 1.0e+INF)
      (setq s 0 e 255 f (1- (expt 2 23))))
     ((= value -1.0e+INF)
      (setq s 1 e 255 f (1- (expt 2 23))))
     ((isnan value)
      (setq s 0 e 255 f 1))
     (t
      (setq s (if (>= value 0.0)
		  (progn (setq f value) 0)
		(setq f (* -1 value)) 1))
      (while (>= (* f (expt 2.0 e)) 2.0) (setq e (1- e)))
      (if (= e 0) (while (< (* f (expt 2.0 e)) 1.0) (setq e (1+ e))))
      (setq f (round (* (1- (* f (expt 2.0 e))) (expt 2 23)))
	    e (+ (* -1 e) 127))))
    (unibyte-string (+ (lsh s 7) (lsh (logand e #XFE) -1))
		    (+ (lsh (logand e #X01) 7) (lsh (logand f #X7F0000) -16))
		    (lsh (logand f #XFF00) -8)
		    (logand f #XFF))))

(defconst osc-int32-zero (unibyte-string 0 0 0 0))
(defconst osc-most-negative-signed-int32 (- (ash 1 (1- 32))))
(defconst osc-most-positive-signed-int32 (1- (ash 1 (1- 32))))
(defconst osc-most-positive-unsigned-int32 (1- (ash 1 32)))

(defun osc-int32 (integer &optional unsigned)
  (cl-check-type integer integer)
  (if (zerop integer)
      osc-int32-zero
    (if unsigned
	(unless (and (natnump integer)
		     (<= integer osc-most-positive-unsigned-int32))
	  (signal 'overflow-error (list integer)))
      (unless (and (>= integer osc-most-negative-signed-int32)
		   (<= integer osc-most-positive-signed-int32))
	(signal 'overflow-error (list integer)))
      (unless (natnump integer)
	(setq integer (+ integer (ash 1 32)))))
    (let (bytes)
      (dotimes (_ 4)
	(push (logand integer #xFF) bytes)
	(setq integer (ash integer -8)))
      (apply #'unibyte-string bytes))))

(defconst osc-timetag-now
  (concat (osc-int32 0 t) (osc-int32 1 t)))

(defconst osc-ntp-offset
  (round
   (float-time (time-subtract (time-convert 0)
			      '(-33707 33152)))))

(defun osc-timetag (&optional time)
  (if (not time)
      osc-timetag-now
    (pcase (time-convert time 'list)
      (`(,high ,low ,usec ,psec)
       (concat (osc-int32 (+ (ash high 16) low osc-ntp-offset)
			  t)
	       (osc-int32 (round (* (+ (* usec 1e-06) (* psec 1e-12))
				    (1- (ash 1 32))))
			  t))))))

;;;###autoload
(defun osc-make-client (host port)
  "Create an OSC client process which talks to HOST and PORT."
  (make-network-process
   :name "OSCclient"
   :coding 'binary
   :host host
   :service port
   :type 'datagram))

(defun osc-make-message (path &rest args)
  "Make a OSC message with specified PATh and ARGS.
A unibyte string is returned.  Use `vconcat' to convert that unibyte
string to a vector if embedding in another OSC message is what you want."
  (apply
   'concat
    (osc-string path)
    (osc-string
     (apply
      'concat ","
      (mapcar (lambda (arg)
		(cond
		 ((floatp arg) "f")
		 ((integerp arg) "i")
		 ((stringp arg) "s")
		 ((vectorp arg) "b")
		 (t (error "Invalid argument: %S" arg))))
	      args)))
    (mapcar
     (lambda (arg)
       (cond
	((floatp arg) (osc-float32 arg))
	((integerp arg) (osc-int32 arg))
	((stringp arg) (osc-string arg))
	((vectorp arg) (osc-blob arg))))
     args)))

;;;###autoload
(defun osc-send-message (process path &rest args)
  "Send an OSC message from PROCESS to the specified PATH with ARGS."
  (process-send-string process (apply #'osc-make-message path args)))

(defconst osc-bundle-tag (osc-string "#bundle"))

(defun osc-make-bundle (time &rest messages)
  "Make a bundle with timetag TIME and MESSAGES as payload."
  (apply #'concat osc-bundle-tag (osc-timetag time)
	 (mapcar (lambda (message)
		   (concat (osc-int32 (length message)) message))
		 messages)))

;;;###autoload
(defun osc-send-bundle (process time &rest messages)
  "Send a bundle to PROCESS with timetag TIME and MESSAGES as payload."
  (process-send-string process (apply #'osc-make-bundle time messages)))

(defun osc-read-string ()
  (let ((pos (point)) string)
    (while (not (= (following-char) 0)) (forward-char 1))
    (setq string (buffer-substring-no-properties pos (point)))
    (forward-char (- 4 (% (length string) 4)))
    string))

(defun osc-read-blob ()
  (let* ((length (osc-read-int32))
	 (pos (point))
	 (vector (progn
		   (forward-char length)
		   (string-to-vector (buffer-substring pos (point)))))
	 (padding (% (- 4 (% length 4)) 4)))
    (forward-char padding)
    vector))

(defun osc-read-int32 ()
  (let ((value 0))
    (dotimes (_ 4)
      (setq value (logior (* value 256) (following-char)))
      (forward-char 1))
    value))

(defun osc-read-float32 ()
  (let ((s (lsh (logand (following-char) #X80) -7))
	(e (+ (lsh (logand (following-char) #X7F) 1)
	      (lsh (logand (progn (forward-char) (following-char)) #X80) -7)))
	(f (+ (lsh (logand (following-char) #X7F) 16)
	      (lsh (progn (forward-char) (following-char)) 8)
	      (prog1 (progn (forward-char) (following-char)) (forward-char)))))
    (cond
     ((and (= e 0) (= f 0))
      (* 0.0 (expt -1 s)))
     ((and (= e 255) (or (= f (1- (expt 2 23))) (= f 0)))
      (* 1.0e+INF (expt -1 s)))
     ((and (= e 255) (not (or (= f 0) (= f (1- (expt 2 23))))))
      0.0e+NaN)
     (t
      (* (expt -1 s)
	 (expt 2.0 (- e 127))
	 (1+ (/ f (expt 2.0 23))))))))

(defun osc-read-float64 ()
  (let ((s (lsh (logand (following-char) #X80) -7))
	(e (+ (lsh (logand (following-char) #X7F) 4)
	      (lsh (logand (progn (forward-char) (following-char)) #XF0) -4)))
	(f (+ (lsh (logand (following-char) #X0F) 48)
	      (lsh (progn (forward-char) (following-char)) 40)
	      (lsh (progn (forward-char) (following-char)) 32)
	      (lsh (progn (forward-char) (following-char)) 24)
	      (lsh (progn (forward-char) (following-char)) 16)
	      (lsh (progn (forward-char) (following-char)) 8)
	      (prog1 (progn (forward-char) (following-char)) (forward-char)))))
    (cond
     ((and (= e 0) (= f 0))
      (* 0.0 (expt -1 s)))
     ((and (= e 2047) (or (= f (1- (expt 2 52))) (= f 0)))
      (* 1.0e+INF (expt -1 s)))
     ((and (= e 2047) (not (or (= f 0) (= f (1- (expt 2 52))))))
      0.0e+NaN)
     (t
      (* (expt -1 s)
	 (expt 2.0 (- e 1023))
	 (1+ (/ f (expt 2.0 52))))))))

(defun osc-read-timetag ()
  (let ((secs (osc-read-int32)) (frac (osc-read-int32)))
    (if (and (zerop secs) (= frac 1))
	nil ; now
      (time-add (time-convert (- secs osc-ntp-offset))
		(time-convert (cons frac (ash 1 32)))))))

(defun osc-server-set-handler (server path handler)
  "Set HANDLER for PATH on SERVER.
IF HANDLER is nil, remove previously defined handler and fallback to
the generic handler for SERVER."
  (let* ((handlers (plist-get (process-plist server) :handlers))
	 (slot (assoc-string path handlers)))
    (if slot
	(setcdr slot handler)
      (plist-put
       (process-plist server)
       :handlers (nconc (list (cons path handler)) handlers)))))

(defun osc-server-get-handler (server path)
  (or (cdr (assoc path (plist-get (process-plist server) :handlers)))
      (plist-get (process-plist server) :generic)))

(defun osc-filter (proc string)
  (when (= (% (length string) 4) 0)
    (with-temp-buffer
      (set-buffer-multibyte nil)
      (insert string)
      (goto-char (point-min))
      (let ((path (osc-read-string)))
	(if (not (string= path "#bundle"))
	    (when (= (char-after) ?,)
	      (save-excursion
		(apply (osc-server-get-handler proc path)
		       path
		       (mapcar
			(lambda (type)
			  (cl-case type
			    (?b (osc-read-blob))
			    (?d (osc-read-float64))
			    (?f (osc-read-float32))
			    (?i (osc-read-int32))
			    (?s (osc-read-string))))
			(string-to-list (substring (osc-read-string) 1))))))
	  (let ((time (osc-read-timetag)))
	    (while (not (eobp))
	      (let* ((size (osc-read-int32))
		     (data (buffer-substring
			    (point) (progn (forward-char size) (point)))))
		(run-at-time time nil #'osc-filter proc data)))))))))

;;;###autoload
(defun osc-make-server (host port default-handler)
  "Create an OSC server which listens on HOST and PORT.
DEFAULT-HANDLER is a function with arguments (path &rest args) which is called
when a new OSC message arrives.  See `osc-server-set-handler' for more
fine grained control.
A process object is returned which can be dicarded with `delete-process'."
  (make-network-process
   :name "OSCserver"
   :coding 'binary
   :filter #'osc-filter
   :host host
   :service port
   :server t
   :type 'datagram
   :plist (list :generic default-handler)))

(defun osc--test-transport-equality (value)
  "Test if transporting a certain VALUE via OSC results in equality.
This is mostly for testing the implementation robustness."
  (let* ((osc-test-value value)
	 (osc-test-func (cond ((or (floatp value) (integerp value)) '=)
			      ((stringp value) 'string=)))
	 (osc-test-done nil)
	 (osc-test-ok nil)
	 (server (osc-make-server
		  "localhost" t
		  (lambda (_path v)
		    (setq osc-test-done t
			  osc-test-ok (list v (funcall osc-test-func
						       osc-test-value v))))))
	 (client (osc-make-client
		  (nth 0 (process-contact server)) (nth 1 (process-contact server)))))
    (osc-send-message client
		      "/test" osc-test-value)
    (while (not osc-test-done)
      (accept-process-output server 0 500))
    (delete-process server) (delete-process client)
    osc-test-ok))

(provide 'osc)
;;; osc.el ends here

;;; prf-wysiwyg-text-mode.el --- Toggle WYSIWYG view in text-mode

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: processes, terminals
;; URL: https://github.com/p3r7/prf-wysiwyg-text-mode
;; Package-Requires: ((cl-lib "0.6.1"))
;;
;; Permission is hereby granted to use and distribute this code, with or
;; without modifications, provided that this copyright notice is copied with
;; it. Like anything else that's free, lusty-explorer.el is provided *as is*
;; and comes with no warranty of any kind, either expressed or implied. In no
;; event will the copyright holder be liable for any damages resulting from
;; the use of this software.

;;; Commentary:
;;  -----------
;;
;; For detailed instructions, please look at the README.md

;;; Code:



;; REQUIRES

(require 'pickling)
(require 'mixed-pitch)



;; VARS

(defcustom prf-wysiwyg-text-mode-lighter " WYSIWYG"
  "String to display in the mode line when md-shiatsu mode is active.

\(When the string is not empty, make sure that it has a leading space.)"
  :tag "prf-wysiwyg lighter"            ; To separate it from `global-...'
  :group 'prf-wysiwyg
  :type 'string)

(defvar prf/org/wysiwyg-vars-og-pickle nil)
(defvar prf/org/wysiwyg-vars-pickle
  '((org-startup-indented . t)
    (org-bullets-bullet-list . '(" "))
    (org-pretty-entities . t)
    (org-hide-emphasis-markers . t)
    (org-agenda-block-separator . "")
    (org-fontify-whole-heading-line . t)
    (org-fontify-done-headline . t)
    (org-fontify-quote-and-verse-blocks . t)))

(defvar prf/org/wysiwyg-faces-og-pickle nil)
(defvar prf/org/wysiwyg-faces-pickle
  '((org-document-title ((:inherit . variable-pitch)
			 (:height . 1.3)))
    (org-level-1 ((:inherit . variable-pitch)
		  (:height . 1.3)))
    (org-level-2 ((:inherit . variable-pitch)
		  (:height . 1.2)))
    (org-level-3 ((:inherit . variable-pitch)
		  (:height . 1.1)))
    (org-level-4 ((:inherit . variable-pitch)
		  (:height . 1.1)))
    (org-level-5 ((:inherit . variable-pitch)
		  (:height . 1.1)))
    (org-level-6 ((:inherit . variable-pitch)
		  (:height . 1.1)))
    (org-level-7 ((:inherit . variable-pitch)
		  (:height . 1.1)))
    (org-level-8 ((:inherit . variable-pitch)
		  (:height . 1.1)))))

(defvar prf/org/wysiwyg-minor-modes-og-pickle nil)
(defvar prf/org/wysiwyg-minor-modes-pickle
  '((mixed-pitch-mode . t)
    ;; (writeroom-mode . t)
    ;; (hl-line-mode . nil)
    ))



;; MODE DEFINITION

(define-minor-mode prf-wysiwyg-text-mode
  "Eye candy for text modes"
  :lighter prf-wysiwyg-text-mode-lighter
  :init-value nil
  :global nil
  (if prf-wysiwyg-text-mode
      (prf-wysiwyg-text-mode--enable)
    (prf-wysiwyg-text-mode--disable)))



;; ENABLE / DISABLE FUNCTIONS

(defun prf-wysiwyg-text-mode--enable ()
  (message "Enabled WYSIWYG")
  (setq prf/org/wysiwyg-vars-og-pickle
  	(pickle-var-list prf/org/wysiwyg-vars-pickle))
  (setq prf/org/wysiwyg-faces-og-pickle
  	(pickle-face-list prf/org/wysiwyg-faces-pickle))
  (setq prf/org/wysiwyg-minor-modes-og-pickle
  	(pickle-minor-mode-list prf/org/wysiwyg-minor-modes-pickle))
  (unpickle-var-list prf/org/wysiwyg-vars-pickle)
  (unpickle-face-list prf/org/wysiwyg-faces-pickle)
  (unpickle-minor-mode-list prf/org/wysiwyg-minor-modes-pickle)
  (when font-lock-mode
    (with-no-warnings (font-lock-fontify-buffer))))


(defun prf-wysiwyg-text-mode--disable ()
  (unpickle-var-list prf/org/wysiwyg-vars-og-pickle)
  (unpickle-face-list prf/org/wysiwyg-faces-og-pickle)
  (unpickle-minor-mode-list prf/org/wysiwyg-minor-modes-og-pickle)
  (when font-lock-mode
    (with-no-warnings (font-lock-fontify-buffer))))




(provide 'prf-wysiwyg-text-mode)

;;; prf-wysiwyg-text-mode.el ends here

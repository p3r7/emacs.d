;;; wysiwyg-text-mode.el --- Toggle WYSIWYG view in text-mode

;; Copyright (C) 2019-2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: processes, terminals
;; URL: https://github.com/p3r7/wysiwyg-text-mode
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

(defcustom wysiwyg-text-mode-lighter " WYSIWYG"
  "String to display in the mode line when wysiwyg-text-mode mode is active.

\(When the string is not empty, make sure that it has a leading space.)"
  :tag "wysiwyg lighter"            ; To separate it from `global-...'
  :group 'wysiwyg
  :type 'string)

(defvar-local wysiwyg-text-mode-vars-og-pickle nil)
(defvar wysiwyg-text-mode-vars-pickle
  '((org-startup-indented . t)
    (org-bullets-bullet-list . '(" "))
    (org-pretty-entities . t)
    (org-hide-emphasis-markers . t)
    (org-agenda-block-separator . "")
    (org-fontify-whole-heading-line . t)
    (org-fontify-done-headline . t)
    (org-fontify-quote-and-verse-blocks . t)))

(defvar-local wysiwyg-text-mode-faces-remap-cookies nil)
;; (defvar wysiwyg-text-mode-faces-og-pickle
;;   '((org-document-title
;;      (:inherit . nil)
;;      (:height . nil))
;;     (org-level-1
;;      (:inherit . nil)
;;      (:height . nil))
;;     (org-level-2
;;      (:inherit . nil)
;;      (:height . nil))
;;     (org-level-3
;;      (:inherit . nil)
;;      (:height . nil))
;;     (org-level-4
;;      (:inherit . nil)
;;      (:height . nil))
;;     (org-level-5
;;      (:inherit . nil)
;;      (:height . nil))
;;     (org-level-6
;;      (:inherit . nil)
;;      (:height . nil))
;;     (org-level-7
;;      (:inherit . nil)
;;      (:height . nil))
;;     (org-level-8
;;      (:inherit . nil)
;;      (:height . nil))))
(defvar wysiwyg-text-mode-faces-pickle
  '((org-document-title
     (:inherit . variable-pitch)
     (:height . 1.3))
    (org-level-1
     (:inherit . variable-pitch)
     (:height . 1.3))
    (org-level-2
     (:inherit . variable-pitch)
     (:height . 1.2))
    (org-level-3
     (:inherit . variable-pitch)
     (:height . 1.1))
    (org-level-4
     (:inherit . variable-pitch)
     (:height . 1.1))
    (org-level-5
     (:inherit . variable-pitch)
     (:height . 1.1))
    (org-level-6
     (:inherit . variable-pitch)
     (:height . 1.1))
    (org-level-7
     (:inherit . variable-pitch)
     (:height . 1.1))
    (org-level-8
     (:inherit . variable-pitch)
     (:height . 1.1))))

(defvar-local wysiwyg-text-mode-minor-modes-og-pickle nil)
(defvar wysiwyg-text-mode-minor-modes-pickle
  '((mixed-pitch-mode . t)
    ;; (writeroom-mode . t)
    ;; (hl-line-mode . nil)
    ))



;; MODE DEFINITION

(define-minor-mode wysiwyg-text-mode
  "Eye candy for text modes"
  :lighter wysiwyg-text-mode-lighter
  :init-value nil
  :global nil
  (if wysiwyg-text-mode
      (wysiwyg-text-mode--enable)
    (wysiwyg-text-mode--disable)))



;; ENABLE / DISABLE FUNCTIONS

;; TODO: https://emacs.stackexchange.com/questions/7281/how-to-modify-face-for-a-specific-buffer
;; TODO: https://stackoverflow.com/questions/4462126/emacs-minor-mode-for-temporarily-modifying-the-default-face

(defun wysiwyg-text-mode--enable ()
  (message "Enabled WYSIWYG")
  ;; (setq wysiwyg-text-mode-vars-og-pickle
  ;; 	(pickle-var-list wysiwyg-text-mode-vars-pickle))
  ;; (setq wysiwyg-text-mode-faces-og-pickle
  ;; 	(pickle-face-list wysiwyg-text-mode-faces-pickle))
  (setq wysiwyg-text-mode-minor-modes-og-pickle
  	(pickle-minor-mode-list wysiwyg-text-mode-minor-modes-pickle))
  (unpickle-var-list-buff-local wysiwyg-text-mode-vars-pickle)
  (setq wysiwyg-text-mode-faces-remap-cookies
        (unpickle-face-list-buff-local wysiwyg-text-mode-faces-pickle))
  (unpickle-minor-mode-list wysiwyg-text-mode-minor-modes-pickle)
  (when font-lock-mode
    (with-no-warnings (font-lock-fontify-buffer))))


(defun wysiwyg-text-mode--disable ()
  ;; (unpickle-var-list wysiwyg-text-mode-vars-og-pickle)
  (unbuff-local-var-list wysiwyg-text-mode-vars-pickle)
  ;; (unpickle-face-list-buff-local wysiwyg-text-mode-faces-og-pickle)
  (mapc #'face-remap-remove-relative wysiwyg-text-mode-faces-remap-cookies)
  (unpickle-minor-mode-list wysiwyg-text-mode-minor-modes-og-pickle)
  (when font-lock-mode
    (with-no-warnings (font-lock-fontify-buffer))))




(provide 'wysiwyg-text-mode)

;;; wysiwyg-text-mode.el ends here

;;; helm-freedesktop-launch.el --- Helm-based app launcher for XDG desktop entries -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: processes, terminals
;; URL: https://github.com/p3r7/freedesktop-launch
;; Package-Requires: ((emacs "24.1")(cl-lib "0.6.1")(freedesktop-launch "0.1.0"))
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;  -----------
;;
;; For detailed instructions, please look at the README.md

;;; Code:



;; REQUIRES

(require 'helm-source)

(require 'freedesktop-launch)



;; HELM SOURCES

(defvar helm-freedesktop-launch--by-filepath-source
  (helm-build-sync-source "Launch XDG desktop entry by filepath"
    :candidates #'freedesktop-launch-list-filepaths
    :fuzzy-match t
    :action '(("launch" .
               (lambda (_candidate)
                 (let ((marked-candidates (helm-marked-candidates)))
                   (mapc
                    (lambda (e)
                      (freedesktop-launch-by-filepath e))
                    marked-candidates))))
              ("visit" .
               (lambda (_candidate)
                 (let ((marked-candidates (helm-marked-candidates)))
                   (mapc
                    (lambda (e)
                      (find-file e))
                    marked-candidates)))))))



;; COMMANDS

(defun helm-freedesktop-launch-by-filepath ()
  (interactive)
  (helm :sources '(helm-freedesktop-launch--by-filepath-source)
        :buffer "*helm XDG desktop entry (by filepath)*"))




(provide 'helm-freedesktop-launch)

;;; helm-freedesktop-launch.el ends here

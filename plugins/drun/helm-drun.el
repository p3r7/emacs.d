;;; helm-drun.el --- Helm-based app launcher for XDG desktop entries -*- lexical-binding: t; -*-

;; Copyright (C) 2020 Jordan Besly
;;
;; Version: 0.1.0
;; Keywords: processes, terminals
;; URL: https://github.com/p3r7/drun
;; Package-Requires: ((emacs "24.1")(cl-lib "0.6.1")(drun "0.1.0"))
;;
;; SPDX-License-Identifier: MIT

;;; Commentary:
;;  -----------
;;
;; For detailed instructions, please look at the README.md

;;; Code:



;; REQUIRES

(require 'helm-source)

(require 'drun)



;; HELM SOURCES

(defvar helm-drun--by-filepath-source
  (helm-build-sync-source "Launch XDG desktop entry by filepath"
    :candidates #'drun-list-filepaths
    :fuzzy-match t
    :action '(("launch" .
               (lambda (_candidate)
                 (let ((marked-candidates (helm-marked-candidates)))
                   (mapc
                    (lambda (e)
                      (drun-by-filepath e))
                    marked-candidates))))
              ("visit" .
               (lambda (_candidate)
                 (let ((marked-candidates (helm-marked-candidates)))
                   (mapc
                    (lambda (e)
                      (find-file e))
                    marked-candidates)))))))



;; COMMANDS

(defun helm-drun-by-filepath ()
  (interactive)
  (helm :sources '(helm-drun--by-filepath-source)
        :buffer "*helm XDG desktop entry (by filepath)*"))




(provide 'helm-drun)

;;; helm-drun.el ends here

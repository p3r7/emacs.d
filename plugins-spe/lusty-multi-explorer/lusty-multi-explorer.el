;;; lusty-multi-explorer.el --- Select files present across multiple locations -*- lexical-binding: t; -*-
;;
;; Copyright (C) 2026
;;
;; Version: 0.2
;; Keywords: convenience, files, matching, tools
;; Package-Requires: ((emacs "25.1") (lusty-explorer "3.2"))
;;
;;; Commentary:
;;
;; Provides `lusty-multi-file-explorer' and `lusty-multi-find-file'.
;;
;; `lusty-multi-file-explorer' prompts for N root directories, then launches
;; lusty-explorer showing only files/directories present in ALL of them.
;; Upon selection the file is opened from every location.
;;
;; `lusty-multi-find-file' is the convenience command: it automatically
;; collects the `default-directory' of every window's buffer in the current
;; frame and uses those as the roots.  Ideal when you have the same project
;; open on multiple servers in side-by-side windows.
;;
;; Usage:
;;
;;   (require 'lusty-multi-explorer)
;;   M-x lusty-multi-find-file
;;

;;; Code:

(require 'lusty-explorer)
(require 'cl-lib)

(defgroup lusty-multi-explorer nil
  "Select and open files present across multiple locations."
  :group 'lusty-explorer
  :prefix "lusty-multi-")

(defvar lusty-multi--roots nil
  "List of root directories for the current multi-explorer session.
The first element is the \"primary\" root shown in the minibuffer.")

(defun lusty-multi--common-directory-prefix (dir1 dir2)
  "Return the longest common prefix of DIR1 and DIR2 on a / boundary.
Both DIR1 and DIR2 must already end with /."
  (let* ((len (min (length dir1) (length dir2)))
         (i 0)
         (last-sep 0))
    (while (and (< i len) (eq (aref dir1 i) (aref dir2 i)))
      (when (eq (aref dir1 i) ?/)
        (setq last-sep (1+ i)))
      (cl-incf i))
    (substring dir1 0 (if (= i len) i last-sep))))

(defun lusty-multi--go-up-n (dir n)
  "Go up N directory levels from DIR.  Return nil if impossible."
  (let ((result dir)
        (i 0))
    (while (< i n)
      (let ((parent (file-name-directory (directory-file-name result))))
        (if (or (null parent) (string= parent result))
            (setq result nil i n)
          (setq result parent)))
      (cl-incf i))
    result))

(defun lusty-multi--corresponding-dir (current-dir primary-root other-root)
  "Map CURRENT-DIR (navigated from PRIMARY-ROOT) to the corresponding dir under OTHER-ROOT.
Handles downward, upward, and sideways navigation relative to the starting roots."
  (let ((exp-dir (file-name-as-directory (expand-file-name current-dir)))
        (exp-primary (file-name-as-directory (expand-file-name primary-root)))
        (exp-other (file-name-as-directory (expand-file-name other-root))))
    (if (string-prefix-p exp-primary exp-dir)
        ;; At or below root: just append the relative portion.
        (concat exp-other (substring exp-dir (length exp-primary)))
      ;; Above or sideways from root.
      ;; Find common ancestor of current-dir and primary-root,
      ;; count how many levels primary-root is deeper than the common ancestor,
      ;; go up that many levels from other-root, then append the portion of
      ;; current-dir below the common ancestor.
      (let* ((common (lusty-multi--common-directory-prefix exp-dir exp-primary))
             (primary-suffix (substring exp-primary (length common)))
             (levels-up (length (split-string (directory-file-name primary-suffix) "/" t)))
             (dir-suffix (substring exp-dir (length common)))
             (base (lusty-multi--go-up-n exp-other levels-up)))
        (when base
          (concat base dir-suffix))))))

(defun lusty-multi--intersect-all (lists-of-files)
  "Return entries present in ALL lists in LISTS-OF-FILES.
Preserves the order of the first list."
  (if (null (cdr lists-of-files))
      (car lists-of-files)
    (let ((sets (mapcar (lambda (files)
                          (let ((ht (make-hash-table :test 'equal
                                                    :size (length files))))
                            (dolist (f files) (puthash f t ht))
                            ht))
                        (cdr lists-of-files))))
      (cl-loop for f in (car lists-of-files)
               when (cl-every (lambda (ht) (gethash f ht)) sets)
               collect f))))

(defun lusty-multi--file-explorer-matches (path)
  "Compute file matches showing only entries present in all roots.
Intended as a local replacement for `lusty-file-explorer-matches'."
  (let* ((primary-root (car lusty-multi--roots))
         (dir (lusty-normalize-dir (file-name-directory path)))
         (file-portion (file-name-nondirectory path))
         ;; Collect file listings from all roots via directory mapping
         (all-file-lists
          (cl-loop for root in lusty-multi--roots
                   for mapped = (and dir
                                     (lusty-multi--corresponding-dir
                                      dir primary-root root))
                   for d = (and mapped (lusty-normalize-dir mapped))
                   collect (if d (file-name-all-completions "" d) '())))
         ;; Intersection across all locations
         (common (lusty-multi--intersect-all all-file-lists))
         (filtered (lusty-filter-files file-portion common)))
    (if (or (string= file-portion "")
            (string= file-portion "."))
        (sort filtered #'string<)
      (lusty-sort-by-fuzzy-score filtered file-portion))))

;;;###autoload
(defun lusty-multi-file-explorer (roots)
  "Launch LustyExplorer showing only files present in all ROOTS.
ROOTS is a list of directory paths.  Navigate as with
`lusty-file-explorer'.  When a file is selected, it is opened
from every location: the first root in the current window, the
rest each in their own window."
  (interactive
   (let ((dirs '()))
     (while (let ((d (read-directory-name
                      (format "Location %d (empty to finish): "
                              (1+ (length dirs)))
                      nil nil nil)))
              (when (and d (not (string-empty-p d)))
                (push d dirs)
                t)))
     (when (< (length dirs) 2)
       (user-error "Need at least 2 locations"))
     (list (nreverse dirs))))
  (let* ((roots (mapcar (lambda (r)
                          (file-name-as-directory (expand-file-name r)))
                        roots))
         (lusty-multi--roots roots)
         (primary-root (car roots))
         (completing-read-function #'completing-read-default)
         (lusty--active-mode :file-explorer))
    (lusty--define-mode-map)
    (let* ((lusty--ignored-extensions-regex
            (concat "\\(?:" (regexp-opt completion-ignored-extensions) "\\)$"))
           (lusty--ignored-buffer-regex
            (mapconcat 'identity lusty--completion-ignored-regexps "\\|"))
           (minibuffer-local-filename-completion-map lusty-mode-map)
           (file
            (cl-letf (((symbol-function 'lusty-file-explorer-matches)
                       #'lusty-multi--file-explorer-matches))
              (lusty--run 'read-file-name primary-root ""))))
      (when file
        (setq file (expand-file-name file))
        (let* ((file-dir (file-name-directory file))
               (file-name (file-name-nondirectory file))
               (all-files
                (cl-loop for root in roots
                         for mapped-dir = (lusty-multi--corresponding-dir
                                           file-dir primary-root root)
                         when mapped-dir
                         collect (concat mapped-dir file-name))))
          (if (file-directory-p file)
              ;; Open dired in all locations
              (cl-loop for f in all-files
                       for first = t then nil
                       do (if first (dired f) (dired-other-window f)))
            ;; Open file from all locations
            (switch-to-buffer (find-file-noselect (car all-files)))
            (dolist (f (cdr all-files))
              (switch-to-buffer-other-window (find-file-noselect f)))))))))

;;;###autoload
(defun lusty-multi-find-file ()
  "Launch multi-location lusty-explorer using directories of visible windows.
Collects `default-directory' from each window in the current frame,
removes duplicates, and shows only files present in all locations.
Requires at least 2 distinct directories."
  (interactive)
  (let* ((dirs (delete-dups
                (mapcar (lambda (w)
                          (file-name-as-directory
                           (expand-file-name
                            (buffer-local-value 'default-directory
                                                (window-buffer w)))))
                        (window-list nil 'nomini)))))
    (when (< (length dirs) 2)
      (user-error "Need at least 2 windows with distinct directories"))
    (lusty-multi-file-explorer dirs)))

(provide 'lusty-multi-explorer)

;;; lusty-multi-explorer.el ends here

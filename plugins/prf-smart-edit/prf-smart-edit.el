;;; prf-smart-edit.el --- drastic changes of emacs edit behaviour


;; Future improvements:
;; - indent when pasting
;; - delete w/ no save case C-w on empty line
;; - use prog-mode-hook and fallback to list, case version < 24
;; - better multiple comment (same level as previous comment instead of indent)
;;   particularly painfull in elisp
;;   http://endlessparentheses.com/implementing-comment-line.html
;; - kill-region shortcut
;; - backward-kill-word as well ?

;; Several implementations are possible
;; - defadvice, said to be buggy
;; - define another function and override default keys


;; Rectangular selection:
;; provided by C-<ret>, C-x r r (copy-rectangle-to-register), C-x r k (kill-rectangle) & C-x r y (yank-rectangle)
;; TODO: state var telling if last kill was on a rectangle,
;; yank-rectangle instead of regular yank if case

;; [[http://ergoemacs.org/emacs/elisp_all_about_lines.html]]


;; inspo:
;; http://stackoverflow.com/questions/1551854/emacs-comment-region-in-c-mode
;; http://stackoverflow.com/questions/6909292/getting-emacs-m-to-produce-style-comments


;; -------------------------------------------------------------------------

;; CONFIGURATION

(setq prf-smed/indent-after-kill t)


;; -------------------------------------------------------------------------

;; UTILS

;;  - String

(defun string/empty-p (s)
  "predicate: only tabs/spaces on current line
Only works for single line strings"
  ;; if want to select multi-lined string, use [[http://www.emacswiki.org/emacs/MultilineRegexp]]
  (when (or (string-match "^[ \t]*$" s)  (zerop (length s)) )
    't ) )


;; ----------------------------------

;;  - Line


(defun empty-line-p ()
  "predicate: no visible characters in line"
  ;; TODO: get selection and call empty-region-p
  (let
      ((text (buffer-substring (line-beginning-position)
			       (line-end-position) ) ))
    (string/empty-p text)
    )
  )


(defun line/delete ()
  "Same as kill-line but wo/ kill-ring side effect"
  (interactive)
  (delete-region (point) (progn (forward-line 1)
                                (forward-char -1)
                                (point))) )


(defun line/delete-whole ()
  "Same as kill-whole-line but wo/ kill-ring side effect"
  (interactive)
  (delete-region (progn (forward-line 0) (point))
                 (progn (forward-line 1) (point))) )


(defun line/kill-ring-save ()
  "Copy current line in kill-ring"
  (interactive)
  (kill-ring-save (line-beginning-position) (line-beginning-position 2)) )
;; NOTE: we could have an optionnal param to tel wether get \n or not


(defun line/kill-ring-save-trim ()
  "Copy current line in kill-ring, trimming begining spaces and tabs"
  (interactive)
  (if (not (empty-line-p))
      (progn
	(beginning-of-line)
	(kill-ring-save (progn (skip-chars-forward " \t") (point))
			(line-beginning-position 2))
	;;(exchange-point-and-mark)
	(beginning-of-line) ) ) )


(defun line/kill ()
  "kill current line"
  (interactive)
  (if (empty-line-p)
      (line/delete-whole)
    (progn
      (kill-region (line-beginning-position) (line-beginning-position 2))
      (if prf-smed/indent-after-kill
	  (indent-according-to-mode))) ) )


(defun line/kill-trim ()
  "kill current line, trimming begining spaces and tabs"
  (interactive)
  (if (empty-line-p)
      (line/delete-whole)
    (progn
      (beginning-of-line)
      (kill-region (progn (skip-chars-forward " \t") (point))
		   (line-beginning-position 2))
      (if prf-smed/indent-after-kill
	  (indent-according-to-mode)) ) ) )


;; ----------------------------------

;;  - Selection

(defun selection/empty-p ()
  (if (use-region-p)
      (let ((text
	     (buffer-substring (region-beginning) (region-end)) ))
	(string/empty-p text) )
    (message "W: used selection/empty-p while no active region") ) )


(defun selection/delete ()
  "Deletes the active selection"
  (interactive)
  (if (use-region-p)
      (delete-region (region-beginning) (region-end))
    (message "W: used selection/delete while no active region") ) )


(defun selection/kill-ring-save ()
  "kill-ring-save the active region"
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (message "W: used selection/kill-ring-save while no active region") ) )


(defun selection/kill-ring-save-trim ()
  "kill-ring-save the active region, trimming begining spaces and tabs"
  (interactive)
  (if (region-active-p)
      (if (< (mark) (point))
	  (progn
	    ;;(cua-exchange-point-and-mark)
	    ;; TODO: really needed, could just use region-beginning & region-end ?
	    (exchange-point-and-mark)
	    (kill-ring-save (progn (skip-chars-forward " \t") (point))
			    (region-end))
	    (exchange-point-and-mark))
	;; else
	(kill-ring-save (progn (skip-chars-forward " \t") (point))
			(region-end)) )
    (message "W: used selection/kill-ring-save-trim while no active region")
    ) )


(defun selection/kill ()
  "kill-region on active region"
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (message "W: used selection/kill while no active region") ) )


(defun selection/kill-trim ()
  "kill-region on active region, trimming begining spaces and tabs"
  (interactive)
  (if (region-active-p)
      (if (< (mark) (point))
	  (progn
	    (exchange-point-and-mark)
	    (kill-region (progn (skip-chars-forward " \t") (point))
			 (region-end))
	    ;; (exchange-point-and-mark) ) ;; useless
	    )
	;; else
	(kill-region (progn (skip-chars-forward " \t") (point))
		     (region-end)) )
    (message "W: used selection/kill-trim while no active region") ) )


;; -------------------------------------------------------------------------

;; COPY

;; M-w
;; - no selection -> copies line
;; - selection    -> copies it

;; http://ergoemacs.org/emacs/emacs_copy_cut_current_line.html

;; other implementation: http://emacsblog.org/2009/05/18/copying-lines-not-killing/

;; Defadvice implementation

;; (defadvice kill-ring-save (before slickcopy activate compile)
;;   "When called interactively with no active region, copy a single line instead."
;;   (interactive
;;    (if mark-active (list (region-beginning) (region-end))
;;      (list (line-beginning-position)
;;            (line-beginning-position 2)))))


(defun copy-line-or-region ()
  "Copy current line or current text selection."
  (interactive)
  (if (region-active-p)
      (selection/kill-ring-save)
    ;; else
    (line/kill-ring-save-trim) ) )

(global-set-key (kbd "M-w") 'copy-line-or-region)


;; Org-mode version

(defun copy-line-or-region-org ()
  ;; NB: doesn't insert a line (unlike standard version)
  ;; org-copy-special does it for whole subtree, and inserts it perfectly
  "Copy the current line or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-ring-save (region-beginning) (region-end))
    (progn
      (org-beginning-of-line)
      (skip-chars-forward " \t")
      (cua-set-mark)
      (org-end-of-line)
      (copy-line-or-region)
      (org-beginning-of-line)) ) )


;; (add-hook 'org-mode-hook
;;  	  '(lambda()
;; 	     (define-key org-mode-map (kbd "M-w") 'copy-line-or-region-org)  ) )
(define-key org-mode-map (kbd "M-w") 'copy-line-or-region-org)



;; -------------------------------------------------------------------------

;; KILL / CUT

;; C-w
;; - no selection -> kills line
;; - selection    -> kills it


;; Defadvice implementation

;; (defadvice kill-region (before slickcut activate compile)
;;   "When called interactively with no active region, kill a single line instead."
;;   (interactive
;;    (if mark-active (list (region-beginning) (region-end))
;;      (list (line-beginning-position)
;;            (line-beginning-position 2)))))


(defun cut-line-or-region ()
  "Cut the current line or current text selection and trim begining whitespaces."
  (interactive)
  (if (region-active-p)
      (selection/kill)
    ;;else
    (line/kill-trim)
    ;;NOTE: deletes remaining white spaces after kill
    ;; not necessary as it gets deleted when saving
    ;;    (if (not (bolp))
    ;;	(delete-region (point) (progn (skip-chars-forward " \t") (point))))
    )
  )


(global-set-key (kbd "C-w") 'cut-line-or-region)


;; Org-mode version

(defun cut-line-or-region-org ()
  ;; NB: leaves an empty line (unlike standard version)
  ;; org-cut-special does it for whole subtree wo/ leaving an empty line
  "Cut the current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (kill-region (region-beginning) (region-end))
    (progn
      (org-beginning-of-line)
      ;; (skip-chars-forward " \t")
      (org-kill-line) ) ) )

;; (add-hook 'org-mode-hook
;;  	  '(lambda()
;; 	     (define-key org-mode-map (kbd "C-w") 'cut-line-or-region-org) ) )
(define-key org-mode-map (kbd "C-w") 'cut-line-or-region-org)



;; -------------------------------------------------------------------------

;; (TOGGLE) COMMENT

;; C-<f8>
;; - no selection -> kills line
;; - selection    -> kills it


;; Defadvice implementation

;; (defadvice comment-or-uncomment-region (before slickccomment activate compile)
;;   "When called interactively with no active region, kill a single line instead."
;;   (interactive
;;    (if mark-active (list (region-beginning) (region-end))
;;      (list (line-beginning-position)
;;            (line-beginning-position 2)))))
;; - 2nd method: redefine functions and rebind keys


(defun comment-or-uncomment-line-or-region () ;;TODO: rename toggle-comment
  "Toggle comment on the current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (progn
      (comment-or-uncomment-region (line-beginning-position) (line-beginning-position 2))
      (next-visual-line) ) ) )

(global-set-key (kbd "C-<f8>") 'comment-or-uncomment-line-or-region)
(global-set-key (kbd "<C-kp-divide>") 'comment-or-uncomment-line-or-region)


;; C languages

(defun comment-or-uncomment-line-or-region-c ()
  "Toggle comment on the current line, or current text selection."
  (interactive)
  (if (region-active-p)
      (progn
	(if (eq major-mode 'c-mode)
	    (setq comment-style 'multi-line
		  comment-start "/* "
		  comment-end   " */")
	  )
	(comment-or-uncomment-region (region-beginning) (region-end)) )
    (progn
      (if (eq major-mode 'c-mode)
	  (setq comment-style 'indent
		comment-start "// "
		comment-end   "") )
      (comment-or-uncomment-region (line-beginning-position) (line-beginning-position 2))
      (next-visual-line)
      ) ) )

(add-hook 'c-mode-common-hook
 	  '(lambda()
	     (local-set-key (kbd "C-<f8>") 'comment-or-uncomment-line-or-region-c)
	     (local-set-key (kbd "<C-kp-divide>") 'comment-or-uncomment-line-or-region-c)
	     ) )


;; -------------------------------------------------------------------------

;; Auto-indent


;; - after new line

;; NB: for c-mode-common-hook, it gets overriden by the completion advices, and thus must be defined after the later being called
(mapc
 (lambda (mode)
   (let ((mode-hook (intern (concat (symbol-name mode) "-hook"))))
     (add-hook mode-hook (lambda nil (local-set-key (kbd "RET") 'newline-and-indent)))))
 '(ada-mode cperl-mode emacs-lisp-mode html-mode lisp-mode perl-mode
	    web-mode
            php-mode prolog-mode ruby-mode scheme-mode sgml-mode sh-mode sml-mode tuareg-mode web-mode))


;; - after yank
;; [[http://emacswiki.org/emacs/AutoIndentation]]

(dolist (command '(yank yank-pop))
  (eval `(defadvice ,command (after indent-region activate)
	   (and (not current-prefix-arg)
		(member major-mode '(emacs-lisp-mode lisp-mode
						     clojure-mode    scheme-mode
						     haskell-mode    ruby-mode
						     rspec-mode      python-mode
						     c-mode          c++-mode
						     objc-mode       latex-mode
						     php-mode        web-mode
						     java-mode       javascript-mode
						     plain-tex-mode)) ;; useless ?
		(let ((mark-even-if-inactive transient-mark-mode))
		  (indent-region (region-beginning) (region-end) nil))))))



;; -------------------------------------------------------------------------

;; Clean too much spaces and blank lines


(global-set-key (kbd "C-x C-o")
		(lambda()(interactive)
		  (progn
		    (just-one-space)
		    (delete-blank-lines) )))



;; -------------------------------------------------------------------------

;; C-d
;; duplicates line or region, no save in kill ring


(defun duplicate-line-or-region (&optional n)
  "Duplicate current line, or region if active.
With argument N, make N copies.
With negative N, comment out original line and use the absolute value."
  (interactive "*p")
  (let ((use-region (use-region-p)))
    (save-excursion
      (let ((text (if use-region        ;Get region if active, otherwise line
                      (buffer-substring (region-beginning) (region-end))
                    (prog1 (thing-at-point 'line)
                      (end-of-line)
                      (if (< 0 (forward-line 1)) ;Go to beginning of next line, or make a new one
                          (newline))))))
        (dotimes (i (abs (or n 1)))     ;Insert N times, or once if not specified
          (insert text))))
    (if use-region nil                  ;Only if we're working with a line (not a region)
      (let ((pos (- (point) (line-beginning-position)))) ;Save column
        (if (> 0 n)                             ;Comment out original with negative arg
            (comment-region (line-beginning-position) (line-end-position)))
        (forward-line 1)
        (forward-char pos)))))

(global-set-key (kbd "C-d") 'duplicate-line-or-region)



;; -------------------------------------------------------------------------

;; SEARCH / REPLACE

(defun replace-string-whole-buffer ()
  "Whole buffer version of replace-string"
  (interactive)
  (save-excursion
    (beginning-of-buffer)
    (call-interactively 'replace-string)))


;; -------------------------------------------------------------------------

;; NAVIGATION

(defun next-visual-line ()
  "next-line but taking into account visual-line-mode"
  (interactive)
  (let ( (max-col-point (current-column)) )
    (end-of-line)
    (next-line)
    (move-to-column max-col-point)
    )
  )


;; -------------------------------------------------------------------------

;; FILE / BUFFER manipulation

;; TODO: use those instead https://github.com/purcell/emacs.d/blob/master/lisp/init-utils.el

;; [[http://www.cabochon.com/~stevey/blog-rants/my-dot-emacs-file.html]]
;; TODO:
;; - write original name by default
;; - autocomplete
(defun rename-file-and-buffer (new-name)
  "Renames both current buffer and file it is visiting to NEW-NAME."
  (interactive "sNew name: ")
  (let ((name (buffer-name))
        (filename (buffer-file-name)))
    (if (not filename)
        (message "Buffer '%s' is not visiting a file!" name)
      (if (get-buffer new-name)
          (message "A buffer named '%s' already exists!" new-name)
        (progn
          (rename-file name new-name 1)
          (rename-buffer new-name)
          (set-visited-file-name new-name)
          (set-buffer-modified-p nil))))))
(global-set-key "\C-x\ W" 'rename-file-and-buffer)


;; -------------------------------------------------------------------------


(provide 'prf-smart-edit)

;;; prf-smart-edit.el ends here

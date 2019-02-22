

(defcustom md-shiatsu-mode-text " ☯"
  "String to display in the mode line when md-shiatsu mode is active.

\(When the string is not empty, make sure that it has a leading space.)"
  :tag "md-shiatsu mode text"                ; To separate it from `global-...'
  :group 'md-shiatsu
  :type 'string)


(defgroup md-shiatsu-faces nil
  "Faces for highlighting text."
  :prefix "md-shiatsu-"
  :group 'faces)

;; http://ergoemacs.org/emacs/elisp_define_face.html
;; https://www.gnu.org/software/emacs/manual/html_node/elisp/Defining-Faces.html
(defface md-shiatsu-chinese-face
  '((default
      ;; :inherit highlight
      :slant italic)
    (((background dark))
     :foreground "#fff6c4")
    (((background light))
     :foreground "#897400")
    )
  "Face for chinese words."
  :group 'md-shiatsu-faces)
(defface md-shiatsu-japanese-face
  '((default
      ;; :inherit highlight
      :slant italic)
    (((background dark))
     :foreground "#ffb1b1")
    (((background  light))
     :foreground "#760000"))
  "Face for japanese words."
  :group 'md-shiatsu-faces)
;; :background "#333940"
(defface md-shiatsu-fundamental-book-face
  '((default
      ;; :inherit highlight
      :slant italic)
    (((background  dark))
     :foreground "#b7edff")
    (((background  light))
     :foreground "#005d7c"))
  "Face for fundamental TCM books."
  :group 'md-shiatsu-faces)


;; TODO: read about rx helpers
;; https://stackoverflow.com/questions/1942045/natural-order-sort-for-emacs-lisp
(defun prf/md-shiatsu/match-word-list (word-list limit)
  (let ((case-fold-search t))
    ;; NB: we need to copy word-list to prevent sort from altering it
    (re-search-forward
     (concat "\\_<\\("
	     (s-join "\\|"
		     (sort (copy-seq word-list) (lambda (a b) (> (length a) (length b)))))
	     ;; (sort word-list 'string-lessp))
	     "\\)\\_>") limit t)))

(defun prf/md-shiatsu/match-chinese (limit)
  (prf/md-shiatsu/match-word-list prf/md-shiatsu/keywords/chinese limit))
(defun prf/md-shiatsu/match-japanese (limit)
  (prf/md-shiatsu/match-word-list prf/md-shiatsu/keywords/japanese limit))
(defun prf/md-shiatsu/match-fundamental-books (limit)
  (prf/md-shiatsu/match-word-list prf/md-shiatsu/keywords/fundamental-books limit))


;; for the syntax, read doc of var font-lock-keywords
;; http://www.modernemacs.com/post/advanced-syntax/
;; https://nicolas.petton.fr/ressources/presentation-stripped6.html
(setq prf/md-shiatsu/highlights
      '(
	(prf/md-shiatsu/match-chinese 1 'md-shiatsu-chinese-face prepend)
	(prf/md-shiatsu/match-japanese 1 'md-shiatsu-japanese-face prepend)
	(prf/md-shiatsu/match-fundamental-books 1 'md-shiatsu-fundamental-book-face t)
	;; ("\\_<\\(Biao\\|Ho Tu\\|Hun Lun\\|Jing\\|Jing Qi\\|Jing Ye\\|Li\\|Nei\\|Qi\\|Shen\\|Shen Qi\\|Tai Ji Tu\\|Tai Yi\\|Tao\\|Wai\\|Wu\\|Wu Xing\\|Xue\\|Yang\\|Yin\\)\\_>" 1 'md-shiatsu-chinese-face prepend)
	;; ("\\_<\\(Qi\\|Jing\\|Shen\\)\\_>" 1 'md-shiatsu-chinese-face prepend)
	;; ("\\_<\\(Ki\\|Shiatsu\\|Do\\)\\_>" 1 'md-shiatsu-japanese-face prepend)
	("\\(<cn>\\)\\([^<]+?\\)\\(</cn>\\)" 2 'md-shiatsu-chinese-face prepend)
	("\\(<ja>\\)\\([^<]+?\\)\\(</ja>\\)" 2 'md-shiatsu-japanese-face prepend)
	("\\(<fundamental-book>\\)\\([^<]+?\\)\\(</fundamental-book>\\)" 2 'md-shiatsu-fundamental-book-face prepend)
	))

;; TODO: allow getting it from JSON config http://tess.oconnor.cx/2006/03/json.el ; https://emacs.stackexchange.com/questions/27407/accessing-json-data-in-elisp

(setq prf/md-shiatsu/keywords/chinese
      '("Qi"
	"Jing"
	"Tsing"
	"Jing Qi"
	"Shen"
	"Shen Qi"

	"Tao"
	"Tai Yi"
	"Tai Ji Tu"
	"Ho Tu"
	"Hun Lun"
	"Wu"

	"Yin"
	"Yang"

	"Wu Xing"

	;; 6 qualités du ciel
	"Tai Yin"
	"Jue Yin"
	"Shao Yin"
	"Tai Yang"
	"Yang Ming"
	"Shao Yang"

	;; Interface du Yin / Yang
	"Nei"
	"Wai"
	"Biao"
	"Li"
	"Xu"
	"Shi"
	"Han"
	"Re"

	"Jin Ye"

	;; O/V
	"Xin"
	"Xiao Chang"
	"Da Chang"
	"Fei"
	"Gan"
	"Dan"
	"Shen"
	"Wei"
	"Pi"

	"Ming Men"

	;; clair / souillé
	"qing"
	"zhuo"

	;; 8 Principes
	"Ba Gang"
	"Pa Kuang"

	;; 8 Règles Thérapeutiques
	"Ba Fa"
	"Xiao fa"
	"Bu fa"
	"Han fa"
	"He fa"
	"Qing fa"
	"Tu fa"
	"Xia fa"
	"Wen fa"

	;; étiologie
	"Bing Yin"
	"Bing ji"
	"Xie"
	"Gui"
	"Bian zheng qui yin"

	;; 3 Réchauffeurs
	"ru wu"
	"ru ou"
	"ru du"

	;; points
	"Xue"
	"Lo"
	"Luo"
	"Mu"
	"Zi"
	;; - Xi
	"Xi"
	"Hsi"
	"Tsri"
	;; - Su Antique
	"Ting"
	"Iong"
	"Iu"
	"King"
	"Ho"
	;; - 4 Mers
	"Si Hai"
	"Si Hai Xue"

	;; méridiens
	;; - MTM
	"Tsing Kann"
	"Tching Kann"
	"Jing Kan"
	"Jing Jin"

	;; E
	;; - comburatoire
	"Yeung Qi"
	"Qing Qi"
	;; - alimentaire
	"Gu Qi"
	;; - comubro-alimentaire / essentielle
	"Zhong Qi"
	;; - de défense
	"Wei Qi"
	"Oé Qi"
	;; - trophique
	"Iong Qi"
	"Ying Qi"
	;; - congénitale
	"Xian Tian Zhi Qi"
	;; - ancestrale
	"Yuan Qi"
	"shen zhong jing qi"

	;; pouls
	"Oé"
	"Cheu"
	"Seu"
	"Wang Chou Houo"
	))

(setq prf/md-shiatsu/keywords/japanese
      '("Ki"
	"Shiatsu"
	"Do"
	"Kuatsu"

	"hara"

	;; points
	"Iu Ketsu"
	"Mo"
	"Bo Ketsu"
	;; - Xi
	"Geki"
	"Gekki"
	))

(setq prf/md-shiatsu/keywords/fundamental-books
      '("Yi King"
	"Tao Te King"

	"Nei Jing"
	"Nei King"
	"Huangdi Neijing"
	"Suwen"
	"So Ouenn"
	"Lingshu"

	"Nan Jing"
	"Huangdi Bashiyi Nanjing"

	"Shang Han Lun"
	"Wen Bing Xue"
	"Jin Kui Yao Lue"))


;; QUOTING

;; YASnippet might be better: https://superuser.com/a/389305

(defun prf/test-md-match ()
  (let ((str "<mark>This needs a highlight</mark>")
	;; (rgx "\\(<mark>\\)\\(?:.\\|\\)*?\\(</mark>\\)"))
	(rgx "\\(<mark>\\)\\([^<]+?\\)\\(</mark>\\)"))
    (if (string-match rgx str)
	(message (concat (match-string 1 str) "_" (match-string 2 str) "_" (match-string 3 str)))
      (message "no match"))))

(defun prf/md-shiatsu/quote-chinese ()
  (interactive)
  (prf/md-shiatsu/quote-html-tag "cn"))
(defun prf/md-shiatsu/quote-japanese ()
  (interactive)
  (prf/md-shiatsu/quote-html-tag "ja"))

(defun prf/md-shiatsu/quote-html-tag (tagname)
  (prf/md-shiatsu/custom-quote (concat "<" tagname ">") (concat "</" tagname ">")))

(defun prf/md-shiatsu/custom-quote (q-start q-end)
  (when (region-active-p)
    ;; (let ((comment-style 'indent)
    ;; 	  (comment-start q-start)
    ;; 	  (comment-end q-end))
    ;;   (comment-region (region-beginning) (region-end)))
    (if (< (mark) (point))
	(progn
	  (exchange-point-and-mark)
	  (goto-char (region-beginning))
	  (insert q-start)
	  (goto-char (region-end))
	  (insert q-end)
	  (exchange-point-and-mark))
      (goto-char (region-beginning))
      (insert q-start)
      (goto-char (region-end))
      (insert q-end))
    ))


;; MODE DEFINITION

(define-minor-mode prf-md-shiatsu
  "Additional font locks and shortcuts."
  :lighter md-shiatsu-mode-text
  :keymap (let ((map (make-sparse-keymap)))
            (define-key map (kbd "C-c c") 'prf/md-shiatsu/quote-chinese)
            (define-key map (kbd "C-c j") 'prf/md-shiatsu/quote-japanese)
            map)

  (message "Enabled shiatsu mode")
  (font-lock-add-keywords nil prf/md-shiatsu/highlights)
  (if (fboundp 'font-lock-flush)
      (font-lock-flush)
    (when font-lock-mode
      (with-no-warnings (font-lock-fontify-buffer))))

  ;; (cwarn-font-lock-keywords prf-md-shiatsu)
  ;; (font-lock-flush)
  )

(provide 'prf-md-shiatsu)

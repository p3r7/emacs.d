

;; https://emacs.stackexchange.com/questions/7375/can-i-format-cells-in-an-org-mode-table-differently-depending-on-a-formula
;; https://emacs.stackexchange.com/questions/66708/how-to-format-cells-of-orgmode-table-with-colors-according-to-its-string
;; https://orgmode.org/worg/org-tutorials/org-spreadsheet-lisp-formulas.html
;; https://www.raebear.net/computers/emacs-colors/

(require 'dash)
(require 's)




(defvar bom/inventory-file nil)

(defvar bom/mouser-api-key nil)



;; ORG TABLE - TABLE

(defun prf/org/table/headers (table)
  (let* ((header-row (car (org-element-map table 'table-row 'identity)))
         (cells (prf/org/table/row/cells header-row)))
    ;; NB: using an alist to preserve order
    (--map
     (cons (prf/org/table/cell/as-string it) it)
     cells)))

(defun prf/org/table/headers-as-string (table)
  (let* ((header-row (car (org-element-map table 'table-row 'identity))))
    (org-element-map header-row 'table-cell #'prf/org/table/cell/as-string)))

(defun bom-table-p (table)
  (or
   (string= "ee-inventory" (org-element-property :name table))
   (s-ends-with? "-bom" (org-element-property :name table))))



;; ORG TABLE - ROW

(defun prf/org/table/row/cells (table-row)
  (org-element-map table-row 'table-cell #'identity))



;; ORG TABLE - CELL

(defun prf/org/table/cell/coords (cell)
  (list (+ (org-element-property :begin cell) 1)
        (- (org-element-property :end cell) 2)))

(defun prf/org/table/cell/content-coords (cell)
  (list (org-element-property :contents-begin cell)
        (org-element-property :contents-end cell)))

(defun prf/org/table/cell/table (cell)
  (org-element-property :parent (org-element-property :parent cell)))

(defun prf/org/table/cell/as-string (cell)
  (let ((cell-content (car (org-element-contents cell))))
    (if cell-content
        (substring-no-properties cell-content)
      "")))

(defun prf/org/table/cell/col (cell)
  (save-excursion
    (goto-char (org-element-property :contents-begin cell))
    (org-table-get 1 nil)))

(defun prf/org/table/cell/field (cell)
  (save-excursion
    (goto-char (org-element-property :contents-begin cell))
    (org-table-get nil 1)))

(defun prf/org/table/cell/other-col (cell other-col)
  (let* ((table (prf/org/table/cell/table cell))
         (headers (prf/org/table/headers-as-string table))
         (other-col-x (-elem-index other-col headers)))
    (unless other-col-x
      (user-error (concat "Table has no col " other-col)))

    (save-excursion
      (goto-char (org-element-property :contents-begin cell))
      (org-table-get nil (+ 1 other-col-x)))))



;; ORG TABLE - FACE

(defun prf/org/table/cell/make-ov (cell)
  (apply #'make-overlay (prf/org/table/cell/coords cell)))


(defun prf/org/table/ee-cell-color (cell)
  (let* ((cell-content (car (org-element-contents cell)))
         (cell-content-str (if cell-content (substring-no-properties cell-content) ""))
         (col-name (prf/org/table/cell/col cell)))

    (cond
     ((string= col-name "type")
      (cond
       ((s-starts-with? "IC" cell-content-str t)
        '((background-color . "dark slate gray")
          (foreground-color . "white")))

       ((s-starts-with? "HW" cell-content-str t)
        '((background-color . "grey")
          (foreground-color . "black")))

       ((s-starts-with? "knob" cell-content-str t)
        '((background-color . "black")
          (foreground-color . "white")))))

     ((string= col-name "name")
      (cond
       ;; part color - generic

       ((or (s-ends-with? "black" cell-content-str t)
            (string= (prf/org/table/cell/other-col cell "manufacturer") "Davies"))
        '((background-color . "black")
          (foreground-color . "white")))

       ((s-ends-with? "white" cell-content-str t)
        '((background-color . "white")
          (foreground-color . "black")))

       ((s-ends-with? "red" cell-content-str t)
        '((background-color . "red")
          (foreground-color . "white")))

       ((s-ends-with? "grey" cell-content-str t)
        '((background-color . "grey")
          (foreground-color . "black")))

       ((s-ends-with? "yellow" cell-content-str t)
        '((background-color . "yellow")
          (foreground-color . "black")))

       ((s-ends-with? "purple" cell-content-str t)
        '((background-color . "purple")
          (foreground-color . "white")))

       ((s-ends-with? "green" cell-content-str t)
        '((background-color . "green")
          (foreground-color . "black")))

       ((s-ends-with? "blue" cell-content-str t)
        '((background-color . "blue")
          (foreground-color . "white")))

       ((s-ends-with? "orange" cell-content-str t)
        '((background-color . "orange")
          (foreground-color . "white")))

       ((s-ends-with? "cream" cell-content-str t)
        '((background-color . "beige")
          (foreground-color . "black")))

       ((s-ends-with? "brown" cell-content-str t)
        '((background-color . "brown")
          (foreground-color . "white")))

       ((s-ends-with? "pink" cell-content-str t)
        '((background-color . "pink")
          (foreground-color . "black")))

       ;; part color - specific

       ((and (s-contains? "9mm" cell-content-str t)
             (string= (prf/org/table/cell/other-col cell "type") "pot"))
        '((background-color . "pale green")
          (foreground-color . "black")))
       ))
     )
    ))

(defun bom/restyle-table (table)
  (interactive)
    (org-element-map (car bom-tables) 'table-cell
      (lambda (cell)
        (when-let ((ov-face (prf/org/table/ee-cell-color cell)))
          (overlay-put (prf/org/table/cell/make-ov cell) 'face ov-face)))))

(defun bom/restyle-buffer ()
  (interactive)
  (let* ((tree (org-element-parse-buffer))
         (tables (org-element-map tree 'table 'identity))
         (bom-tables (-filter #'bom-table-p tables)))
    (-map #'bom/restyle-table bom-tables)))




;; PROVIDER - MOUSER

(defun bom/mouser/seach-url (term)
  (concat "https://www.mouser.fr/c/?q=" term))




;; PROVIDER - TAYDA

(defun bom/tayda/search-url (term)
  (concat "https://www.taydaelectronics.com/catalogsearch/result/?q=" term))

(defun bom/tayda/component-url (tayda-sku)
  "Get Tayda link to component.
Tayda will redirect to the specific item page when provided an SKU."
  (ee/tayda/search-url tayda-sku))



;; COMPONENT CARD

(defun bom-part--buffer (part)
  (let* ((current-buffer (current-buffer))
         (buf-name
          (format "*BOM Part: %s*" part))
         (buf (get-buffer buf-name)))

    (unless buf
      (setq buf (get-buffer-create buf-name)))

    (with-current-buffer buf
      (bom-part-mode))
    buf))

(define-derived-mode bom-part-mode special-mode "BOM Part"
  "Major mode for BOM part cards.")

(defun bom-part (part)
  (interactive "sPart: ")

  ;; (bom/inventory-file)

  )





(provide 'bom)



;; [[http://emacswiki.org/emacs/KeyboardMacros]]

(fset 'log-trex-parse
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 19 45 62 67108896 5 23 1 25 32 19 44 left left left 67108896 C-left C-left C-left C-left C-left backspace right right right backspace 19 68 69 left left 11 down 1 19 44 left left left 67108896 C-right C-right 23 up 5 25 C-left backspace down 1 11 11 left C-left C-left 67108896 C-right 134217847 right 67108896 C-right 134217847 67108896 C-left C-left 21 134217786 40 45 32 25 right left 32 25 right C-left 67108896 C-right 25 134217849 right return 11 down 1] 0 "%d")) arg)))

(fset 'log-trex-to-orgtable
      (lambda (&optional arg) "Keyboard macro." (interactive "p") (kmacro-exec-ring-item (quote ([1 124 32 5 M-left 124 32 5 32 124 down] 0 "%d")) arg)))


(provide 'init-keyboard-macro)

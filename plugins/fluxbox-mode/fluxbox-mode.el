;;; fluxbox-mode.el -- major mode for editing Fluxbox configuration files
;;
;; $Id: fluxbox-mode.el,v 1.2 2004/11/02 10:07:13 jg Exp $
;;
;; Keywords:	languages, faces
;; Author:	Jens Giessmann
;; e-mail:      jg@handcode.de
;; 
;; It is free software; you can redistribute it and/or modify it 
;; under the terms of the GNU General Public License as published by 
;; the Free Software Foundation; either version 2, or (at your option)
;; any later version. 
;; 
;; It is distributed in the hope that it will be useful, but 
;; WITHOUT ANY WARRANTY; without even the implied warranty of 
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU 
;; General Public License for more details. 
;; 
;; You should have received a copy of the GNU General Public License 
;; along with your copy of Emacs; see the file COPYING.  If not, write
;; to the Free Software Foundation, Inc., 59 Temple Place - Suite 330,
;; Boston, MA 02111-1307, USA. 
;;
;; To enable automatic selection of this mode when appropriate files
;; are visited, add the following to your XEmacs configuration file
;; (init.el):
;;
;;
;; (autoload 'fluxbox-mode "fluxbox-mode" "autoloaded" t) 
;; (add-to-list 'auto-mode-alist '("\\.fluxbox/init$"  . fluxbox-mode)) 
;; (add-to-list 'auto-mode-alist '("\\.fluxbox/menu$"  . fluxbox-mode)) 
;; (add-to-list 'auto-mode-alist '("\\.fluxbox/keys$"  . fluxbox-mode)) 
;;
;;------------------------------------------------------------------------
;;; Code:
;;------------------------------------------------------------------------

;; Requires
(require 'font-lock)
(require 'regexp-opt)
(require 'custom)
;;(require 'cc-mode)

;;------------------------------------------------------------------------
;; Variables 
;;------------------------------------------------------------------------
(defvar fluxbox-mode-version "0.10" 
  "The current version number of fluxbox-mode.") 

(defvar fluxbox-mode-map nil
  "Keymap for fluxbox config mode buffers") 

;;------------------------------------------------------------------------
;; Customize
;;------------------------------------------------------------------------
(defgroup fluxbox nil
  "Major mode for editing fluxbox config-files."
  :prefix "fluxbox-"
  :group 'languages)

(defcustom fluxbox-manual-url "http://fluxbox.sourceforge.net/docs.php"
  "*URL at which to find fluxbox manual."
  :type 'string
  :group 'fluxbox)

;; Define function for browsing manual
(defun fluxbox-browse-manual ()
  "Bring up manual for Fluxbox."
  (interactive)
  (browse-url fluxbox-manual-url)
  )

;;------------------------------------------------------------------------
;; Define keymap
;;------------------------------------------------------------------------
(setq fluxbox-mode-map (make-sparse-keymap))

;; Define shortcut
(define-key fluxbox-mode-map
  "\C-c\C-m"
  'fluxbox-browse-manual)

;;------------------------------------------------------------------------
;; Font lock
;;------------------------------------------------------------------------
(defconst fluxbox-font-lock-keywords
   (list
    ;; Comments
    (list "^\\s-*#.*$" 0 'font-lock-comment-face t)
    (list "^\\s-*!.*$" 0 'font-lock-comment-face t)
    
    ;; init keywords    
    (list (concat                                       
	   "\\(session\.\\|screen0\.\\|\\"
           (regexp-opt '("toolbar." "tab." "slit." "titlebar." "left"
			 ))
           "\\)\\w")
	  
	  1 'font-lock-function-name-face)
    
    (list (concat                                       
	   "\\(\\.\\)\\("
           (regexp-opt '("iconbar" "placement" "direction" "onTop" 
			 "autoHide" "rotatevertical" "width" "alignment" 
			 "height"  "widthPercent" "sloppywindowgrouping" 
			 "desktopwheeling"  "colPlacementDirection" 
			 "edgeSnapThreshold"  "rowPlacementDirection" 
			 "windowPlacement"  "showwindowposition" 
			 "focusNewWindows" "workspaces"  "focusModel" 
			 "rootCommand" "workspacewarping"  "focusLastWindow" 
			 "workspaceNames" "maxOverSlit"  "imageDither" 
			 "strftimeFormat" "fullMaximization"  "left" "right"
			 "tabs" "autoRaiseDelay" "keyFile"  "cacheMax" 
			 "imageDither" "menuFile" "opaqueMove"  "styleFile" 
			 "colorsPerChannel" "doubleClickInterval" 
			 "slitlistFile" "cacheLife" "groupFile"))
           "\\):")
	  2 'font-lock-keyword-face)
    
    ;; keygrabber Keys
    (list (concat                                       
	   "\\(?:^\\|\W\\|\\b\\)\\(Control\\|F[0-9]+\\|\\"
           (regexp-opt '("Mod1" "Tab" "Shift" "Escape"
			 ))"\\)\\W")
	  1 'font-lock-variable-name-face)

    (list (concat                                       
	   "\\(?:\\|\W\\|\\b\\)\\(Control\\|F[0-9]+\\|\\"
           (regexp-opt '("Left" "Right" "Down" "Up"
			 ))"\\)\\s-:")
	  1 'font-lock-variable-name-face)
    

    (list "\\(\\(\\s-[a-z]\\s-\\)+\\([a-z]\\s-\\)*\\):" 
	  1 'font-lock-variable-name-face t)
    
    ;; keygrabber actions
    (list (concat                     
	   "\\(?:\\:\\)\\("
           (regexp-opt '("Workspace" "NextTab" "PrevTab" "NextWindow"  
			 "PrevWindow" "NextWorkspace" "PrevWorkspace"  
			 "LeftWorkspace" "RightWorkspace" "Close" 
			 "KillWindow"  "Minimize" "ShadeWindow" 
			 "StickWindow" "ToggleDecor"  "ToggleTab" 
			 "Raise" "Lower" "HorizontalIncrement"  
			 "HorizontalDecrement" "VerticalIncrement"  
			 "VerticalDecrement" "MaximizeHorizontal"  
			 "MaximizeVertical" "MaximizeWindow" 
			 "SendToWorkspace"  "NudgeDown" "NudgeLeft" 
			 "NudgeRight" "NudgeUp"  "BigNudgeDown" 
			 "BigNudgeLeft" "BigNudgeRight"  "BigNudgeUp" 
			 "AbortKeychain" "ExecCommand" "RootMenu"))
           "\\)\\W")
	  1 'font-lock-function-name-face)

    ;; Menue keywords
    (list (concat
	   "\\[\\("
           (regexp-opt '("begin" "submenu" "exec" "include" "end"
			 "nop" "workspaces" "stylesdir" "config"
			 "reconfig" "reconfigure" "restart" "exit"
			 ))"\\)\\]")
	  1 'font-lock-keyword-face)
    
    ;; Menu Titles
    (list "\\[.*\\]\\s-*(\\(.*\\))" 1 'font-lock-variable-name-face)


    ;; fluxter-Config
    (list "{\\(.*\\)}" 1 font-lock-string-face)

    (list (concat                                       
	   "\\(\\.\\|\\:\\|\\"
           (regexp-opt '("fluxter" "desktop" "columns" 
			 "rows" "window" "workspace" "position"
			 ))
           "\\)\\(:\\|\\.\\)")
	  
	  1 'font-lock-function-name-face)
    
    (list (concat                                       
	   "\\(\\.\\)\\("
           (regexp-opt '("autoConfig" "raised" "focus"
			 "focusStyle" "focus" "desktopChangeButton"
			 "windowMoveButton" "windowFocusButton" 
			 "windowRaiseButton" "bevelWidth" "frame"
			 "colorTo" "active" "inactive" "borderColor"
			 "rootCommand"))
           "\\)\\(:\\|\\.\\)")
	  2 'font-lock-keyword-face)

    )
  "Expressions to highlight in fluxbox config files.")

(put 'fluxbox-mode 'font-lock-defaults 
     '(fluxbox-font-lock-keywords nil nil
				  ((?_ . "w")
				   (?- . "w"))))

;; Buggy....
(make-local-hook 'fluxbox-mode-hook) 
  (add-hook 'fluxbox-mode-hook 
            (lambda nil 
	      (set (make-local-variable 'tab-width) 2)
	      ) nil t) 

;;------------------------------------------------------------------------
;;;###autoload
;;------------------------------------------------------------------------

(defun fluxbox-mode ()
  "Major mode for editing Fluxbox configuration files."
  ;;{fluxbox-mode-map}
  ;;[fluxbox-mode] runs the hook `fluxbox-mode-hook'."
  (interactive)
  (setq font-lock-maximum-decoration t)
  (kill-all-local-variables)
  (make-local-variable 'comment-start)
  (setq comment-start "# ")
  (make-local-variable 'comment-start-skip)
  (setq comment-start-skip "#\\W*")
  (make-local-variable 'comment-column)
  (setq comment-column 48)
  (use-local-map fluxbox-mode-map)
  (setq mode-name "Fluxbox")
  (setq major-mode 'fluxbox-mode)
  (run-hooks 'fluxbox-mode-hook))

;; Provides
(provide 'fluxbox-mode)

;;------------------------------------------------------------------------
;;; fluxbox-mode.el ends here
;;------------------------------------------------------------------------

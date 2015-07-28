; General settings
(setq inhibit-startup-message t)
(show-paren-mode t)
(column-number-mode t)
(global-auto-revert-mode t)
(setq indent-tabs-mode nil)
(put 'downcase-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(semantic-mode 1)

(add-to-list 'default-frame-alist '(fullscreen . maximized))

(setq uniquify-buffer-name-style 'forward)
; Autosave settings
(setq emacs-autosave-dir (concat emacs-tmp-dir "/autosaves/"))
(setq auto-save-list-file-prefix emacs-autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,emacs-autosave-dir t)))

; Remove XEmacs related shenanigans
(if (fboundp 'menu-bar-mode) (menu-bar-mode 0))
(if (fboundp 'tool-bar-mode) (tool-bar-mode 0) )
(if (fboundp 'scroll-bar-mode) (scroll-bar-mode 0))

; Enable winner mode
(when (fboundp 'winner-mode) (winner-mode 1))

; Font
(set-face-attribute 'default nil
                    :family "Source Code Pro"
;;		    :family "Inconsolata"
                    :height 110)
(set-face-attribute 'mode-line nil
                    :family "Source Code Pro"
		    :height 110)

					;
;Browser Support
(setq browse-url-browser-function 'browse-url-generic
      shr-external-browser 'browse-url-generic
      browse-url-new-window-flag t
      browse-url-generic-program "firefox"
      browse-url-generic-args '("--new-window"))

(require 'powerline)

(setq sml/no-confirm-load-theme t)
(setq sml/mode-width 'full)
(setq sml/name-width 40)
(setq sml/theme 'respectful)

;Load theme files
;(defadvice load-theme (before theme-dont-propagate activate)
;  (mapcar #'disable-theme custom-enabled-themes))

;(defadvice load-theme (after theme-dont-propagate activate)
;  (sml/setup))

;(if (>= (display-color-cells) 256)
(load-theme 'badger t)




(provide 'local_configs)

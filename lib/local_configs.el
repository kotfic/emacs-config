; General settings
(setq inhibit-startup-message t)
(show-paren-mode t)
(column-number-mode t)
(global-auto-revert-mode t)
(setq indent-tabs-mode nil)
(setq require-final-newline t)
(put 'downcase-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)
(semantic-mode 1)

(setq x-select-enable-cliboard t
      yank-pop-change-selection t)

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
;;                  :family "Inconsolata"
                    :height 110)
(set-face-attribute 'mode-line nil
                    :family "Source Code Pro"
                    :height 110)

                                        ;
;Browser Support
(setq browse-url-browser-function 'browse-url-generic
      shr-external-browser 'browse-url-generic
      browse-url-new-window-flag t
      browse-url-generic-program "chromium")
;browse-url-generic-args '("--new-window"))


;(setq sml/no-confirm-load-theme t)
;(setq sml/mode-width 'full)
;(setq sml/name-width 40)
;(setq sml/theme 'respectful)

;Load theme files
;(defadvice load-theme (before theme-dont-propagate activate)
;  (mapcar #'disable-theme custom-enabled-themes))

;(defadvice load-theme (after theme-dont-propagate activate)
;  (sml/setup))

;(if (>= (display-color-cells) 256)
;(load-theme 'badger t)


;; coding standards
;; both these lists should be lowercased
(setq no-cleanup-filenames '("makefile" "rules"))
(setq no-cleanup-extensions '("md" "org" "xml" "tsv" "csv" "config" "conf"))

(defun should-cleanup-buffer?()
  "Returns t if the buffer is an actual file, the files extension isn't in no-cleanup-extensions,
and it's name isn't in no-cleanup-filenames."
  (and (buffer-file-name)
       (not (-contains? no-cleanup-filenames (downcase (file-name-nondirectory (buffer-file-name)))))
       (not (and (file-name-extension (buffer-file-name)) ;has a file extension
                 (-contains? no-cleanup-extensions (downcase (file-name-extension (buffer-file-name))))))))

(defun buffer-cleanup()
  "A less safe buffer cleanup, indents everything."
  (interactive)
  (buffer-cleanup-safe)
  (indent-region (point-min) (point-max)))

(defun buffer-cleanup-safe()
  (interactive)
  (when (should-cleanup-buffer?)
    (whitespace-cleanup)
    (untabify (point-min) (point-max))
    (set-buffer-file-coding-system 'utf-8)))

(add-hook 'before-save-hook 'buffer-cleanup-safe)

(defadvice zap-to-char (after zap-until-char (arg char) activate)
  "Makes zap-to-char act like zap-until-char."
  (insert char)
  (backward-char 1))

(setq backup-directory-alist `((".*" . ,(concat emacs-tmp-dir "/autosaves/")))
      auto-save-file-name-transformations `((".*" ,(concat emacs-tmp-dir "/autosaves/") t)))


(defun wrapup ()
  (interactive)
  (let ((projects '("/home/kotfic/org"
                    "/home/kotfic/.emacs.d"
                    "/home/kotfic/.dot")))
    (save-window-excursion
      (mapcar #'(lambda (p)
                  (magit-status p)
                  (delete-other-windows)
                  (recursive-edit))
              projects))))

(provide 'local_configs)

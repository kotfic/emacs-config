;; 
;; TODO investiate Bibretrieve? 
;; TODO Add flycheck in erc
;; TODO smex - make M-x use ido

(defvar emacs-config-dir (expand-file-name "~/.emacs.d"))
(defvar emacs-tmp-dir    (expand-file-name (concat emacs-config-dir "/" "tmp")))

;; Add subdirectories in emacs-config-dir
(let ((default-directory emacs-config-dir))
  (normal-top-level-add-subdirs-to-load-path))


;; adding package information
(require 'package)

(setq package-user-dir (concat emacs-config-dir "/" "packages"))

(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("melpa" . "http://melpa.milkbox.net/packages/")))
(package-initialize)


;; ensure packages are installed

(defvar elpa-required-packages '(s
				 org
				 org-bullets
				 prodigy
				 color-theme-sanityinc-tomorrow
				 ido-ubiquitous
				 magit
				 pandoc-mode
				 twittering-mode 
				 auto-complete
				 autopair
				 flycheck
				 epc
				 jedi
				 ein
				 restclient
				 ))

;; load or install packages
(dolist (pkg elpa-required-packages)
  (if (not (package-installed-p pkg))
      (package-install pkg)))


; TODO - Need to figure out how to download org from elpa
; see (mapcar 'string-to-number (split-string (org-version) "[.]")
; get check for package description stuff to see if elpa version
; is higher than installed version



(require 'use-package)

(use-package s)

(use-package dash)

(use-package request)

; IDO
(use-package ido
	     :init (progn
		     (ido-mode 'both) ; for buffers and files
		     (setq
		      ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
		      ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace" "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
		      ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
		      ido-case-fold  t                 ; be case-insensitive
		      ido-enable-last-directory-history t ; remember last used dirs
		      ido-max-work-directory-list 30   ; should be enough
		      ido-max-work-file-list      50   ; remember many
		      ido-use-filename-at-point nil    ; don't use filename at point (annoying)
		      ido-use-url-at-point nil         ; don't use url at point (annoying)
		      ido-max-prospects 10             ; don't spam my minibuffer
		      ido-confirm-unique-completion nil)

                                        ; when using ido, the confirmation is rather annoying...
		     (setq confirm-nonexistent-file-or-buffer nil)
		     (fset 'dired 'ido-dired)
		     
		     ))

; IDO Ubiquitous
; TODO - need to figure out some of the org related configurations
(use-package ido-ubiquitous
	     :init (progn
		     (ido-ubiquitous-mode 1)))
	     
; Uniquify
(use-package uniquify
	     :init (progn
		     (setq uniquify-buffer-name-style 'forward)))

; Windmove
(use-package windmove
	     :config (progn
		       (windmove-default-keybindings 'shift)))

; Tramp
(use-package tramp
	     :config (progn
		       (setq tramp-default-method "ssh")))

; Magit
(use-package magit
	     :bind (("C-x g" . magit-status)))


; Twittering Mode
(use-package twittering-mode
	     :commands (twit twittering-node) 
	     :config (progn
		       (setq twittering-use-master-password t
			     twittering-cert-file "/etc/ssl/certs/ca-certificates.crt")))

; LaTeX


;; Flycheck
(use-package flycheck)
		     


; Auto complete
(use-package auto-complete
	     :config (progn
		       (setq
			ac-auto-start 2
			ac-override-local-map nil
			ac-use-menu-map t
			ac-candidate-limit 20)))



; Jedi
; TODO Jedi goto-definition is not working.
(use-package jedi
	     :defer t
	     :bind (("C-c d" . jedi:show-doc)
		    ("M-SPC" . jedi:complete)
		    ("M-." . jedi:goto-definition))

	     :init (progn
		     (defun pp:custom-jedi-setup ()
		       (jedi:setup)
		       (jedi:ac-setup)))

	     :config (progn
		       (setq jedi:server-command
			     `("python2" ,(concat jedi:source-dir "jediepcserver.py")))
		       
		       (setq jedi:setup-keys t
			     jedi:tooltip-method nil
			     jedi:get-in-function-call-delay 300
			     jedi:complete-on-dot t)))



; Python
(use-package python
	     :commands python-mode
	     :config (progn
		       (setq pylint:epylint-executable "epylint"
			     python-shell-interpreter "ipython"
			     python-shell-interpreter-args "-i"
			     python-shell-buffer-name "Python"
			     python-shell-prompt-regexp "In \\[[0-9]+\\]: "
			     python-shell-prompt-block-regexp ":"
			     python-shell-prompt-output-regexp "Out\\[[0-9]+\\]: ")
		     
		       ; auto pair
		       (use-package autopair)
				    
		       ; adding hooks
 		       (add-hook 'python-mode-hook (lambda ()
						     (unless (tramp-tramp-file-p (buffer-file-name))
						       (flycheck-mode))))

		       ; hooks
		       (add-hook 'python-mode-hook 'auto-complete-mode)
		       (add-hook 'python-mode-hook 'autopair-mode)
		       (add-hook 'python-mode-hook 'pp:custom-jedi-setup)))

		       (add-hook 'inferior-python-mode-hook 'auto-complete-mode)
		       (add-hook 'inferior-python-mode-hook 'autopair-mode)
		       (add-hook 'inferior-python-mode-hook 'pp:custom-jedi-setup)
		       
		       
; Prodigy
(use-package prodigy
  :commands prodigy
  :bind (("<f12>" . prodigy))
  :config (progn
	    ; (add-hook 'prodigy-mode-hook 'virtualenv-minor-mode)
	    

	    (load "prodigy_python.el")


	    (prodigy-define-service
	      :name "General IPython2 Kernel"
	      :command "ipython2"
	      :args '("kernel")
	      :tags '(python_kernel)
	      :stop-signal 'sigquit )

	    (prodigy-define-service
	      :name "General Python2 Notebook"
	      :command "ipython2"
	      :args '("notebook" "--no-browser")
	      ; :init (lambda () (virtualenv-workon "pp_twitter"))
	      :tags '(python_notebook))


	    ))

; Yasnippet

; ERC
(use-package erc
	     :commands erc
	     :config (progn
		       (erc-track-mode t)
		       (setq erc-track-when-inactive t
			     erc-hide-list '("JOIN" "PART" "QUIT" "NICK"))

		       (use-package erc-notify
				    :init (progn
					    (erc-notify-mode t)
					    (setq erc-notify-list '("danlamanna" "danlamanna^" "JDHankle")))
			     )))





; Doc View Mode
(use-package doc-view
  :mode (("\\.docx\\'" . doc-view-mode)
	 ("\\.doc\\'" . doc-view-mode)
	 ("\\.odt\\'" . doc-view-mode))
  :config (progn
	    (setq doc-view-continuous t)
	    (setq doc-view-resolution 300)
	    (defun doc-view-rotate-current-page ()
	      "Rotate the current page by 90 degrees.
Requires ImageMagick installation"
	      (interactive)
	      (when (eq major-mode 'doc-view-mode)
		;; we are assuming current doc-view internals about cache-names
		(let ((file-name (expand-file-name (format "page-%d.png" (doc-view-current-page)) (doc-view-current-cache-dir))))
		  ;; assume imagemagick is installed and rotate file in-place and redisplay buffer
		  (call-process-shell-command "convert" nil nil nil "-rotate" "90" file-name file-name)
		  (clear-image-cache)
		  (doc-view-goto-page (doc-view-current-page)))))

	    (defun doc-view-pdftotext ()
	      "Run pdftotext on the entire buffer."
	      (interactive)
	      (let ((modified (buffer-modified-p)))
		(erase-buffer)
		(shell-command
		 (concat "pdftotext " (buffer-file-name) " -")
		 (current-buffer)
		 t)
		(set-buffer-modified-p modified)))


	    (defadvice scroll-other-window (around doc-view-scroll-up-or-next-page activate)
	      "When next buffer is `doc-view-mode', do `doc-view-scroll-up-or-next-page'."
	      (other-window +1)
	      (if (eq major-mode 'doc-view-mode)
		  (let ((arg (ad-get-arg 0)))
		    (if (null arg)
			(doc-view-scroll-up-or-next-page)
		      (doc-view-next-line-or-next-page arg))
		    (other-window -1))
		(other-window -1)
		ad-do-it))

	    (defadvice scroll-other-window-down (around doc-view-scroll-down-or-previous-page activate)
	      "When next buffer is `doc-view-mode', do `doc-view-scroll-down-or-previous-page'."
	      (other-window +1)
	      (if (eq major-mode 'doc-view-mode)
		  (let ((arg (ad-get-arg 0)))
		    (if (null arg)
			(doc-view-scroll-down-or-previous-page)
		      (doc-view-previous-line-or-previous-page arg))
		    (other-window -1))
		(other-window -1)
		ad-do-it))


))
	    


; Org Mode
(use-package org
	     :commands org-mode
	     :bind (("C-c l" . org-store-link)
		    ("C-c c" . org-capture)
		    ("C-c a" . org-agenda)
		    ("C-c b" . org-iswitchb))

	     :config (progn

		       (use-package org-compat :if window-system)
				    
		       (setq org-log-done 'time
			     org-use-tag-inheritance nil
			     org-hide-leading-stars t
			     org-startup-indented t
			     org-export-backends '(ascii html icalendar latex md odt)
			     org-startup-with-inline-images "inlineimages"
			     org-format-latex-options '(:foreground default 
								    :background default 
								    :scale 1.4 
								    :html-foreground "Black" 
								    :html-background "Transparent" 
								    :html-scale 1.0 
								    :matchers ("begin" "$1" "$" "$$" "\\(" "\\[")))

					; Create a custom ID on links so you can move them around and they still work
		       (use-package org-id
				    :config (progn
					      (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
						    org-link-to-org-use-id 'create-if-interactive-and-no-custom-id)))



					; clock sum
		       (setq org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t))

		       ;; Make windmove work in org-mode:
		       (add-hook 'org-shiftup-final-hook 'windmove-up)
		       (add-hook 'org-shiftleft-final-hook 'windmove-left)
		       (add-hook 'org-shiftdown-final-hook 'windmove-down)
		       (add-hook 'org-shiftright-final-hook 'windmove-right)


			; Agenda Functions

		       (setq org-agenda-files (concat emacs-config-dir "/.agenda-files" ))
		       (setq org-agenda-todo-ignore-with-date t)

                       ; could be useful
;		       (if (file-exists-p (expand-file-name "~/org/.agenda-files"))
;			   (setq org-agenda-files "~/org/.agenda-files" ))


					; ensure that tags-todo do not show scheduled items
		       (setq org-agenda-tags-todo-honor-ignore-options t)

					; don't show scheduled items
		       (setq org-agenda-skip-deadline-prewarning-if-scheduled t)

		       (setq org-agenda-custom-commands '(
							  ("n" "Agenda and TODO's"
							   ((agenda "")
							    (tags-todo "+urgent")
							    ))
							  ("P" "Process Improvements"
							   ((tags-todo "CATEGORY=\"Process\"-backburner")
							    (tags-todo "CATEGORY=\"Process\"+backburner")
							    ))
							  ))



                      ; Capture
		       (setq org-default-notes-file (concat org-directory "/unfiled.org"))
		       (define-key global-map "\C-cr" 'org-capture)

		       (setq org-refile-targets '((org-agenda-files . (:maxlevel . 3))))
		       (setq org-refile-use-outline-path 'file)
		       (setq org-refile-path-complete-in-steps t)


		       (setq org-capture-templates 
			     '(("t" "TODO" entry (file+headline "~/org2/unfiled.org" "Tasks")
				"* TODO %?\n  %i\n  %a")
			       ("e" "Email" entry (file+headline "~/org2/unfiled.org" "Email")
				"* EMAIL %?\n %i\n %a ")
			       ("q" "Question" entry (file+headline "~/org2/rpad705.org" "Questions")
				"* QUESTION %?\n %i\n %a ")
			       ("c" "Clock in" entry (file+headline "~/org2/unfiled.org" "Time")
				"* %?\n %i\n %a" :clock-in t :clock-keep t)
			       ))

		       ; Org Export
		       (setq org-export-odt-preferred-output-format "docx"
			     org-export-odt-styles-file nil
			     org-file-apps '((auto-mode . emacs)
					     ("\\.mm\\'" . default)
					     ("\\.x?html?\\'" . default)
					     ("\\.pdf\\'" . "/usr/bin/xpdf %s")))

		       (setq org-latex-pdf-process 
			     '("latexmk -c"
			       "pdflatex -interaction nonstopmode -output-directory %o %f" 
			       "biber --trace %b.bcf"
			       "pdflatex -interaction nonstopmode -output-directory %o %f"))

		       
		       ; Org Babel

		       (setq org-src-preserve-indentation t)
		       (setq org-src-fontify-natively t)
		       (setq org-confirm-babel-evaluate nil)


		       (org-babel-do-load-languages
			'org-babel-load-languages
			'((emacs-lisp . t)
			  (python . t)
			  (R . t)
			  (latex . t)
			  (sh . t )
			  (sql . t)
			  (ditaa .t)))

		       (org-babel-lob-ingest "~/.emacs.d/lib/org/doc/library-of-babel.org")


		       ; Org Bullets
		       (use-package org-bullets :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

		       
		       ; Windmove conflicts
		       (add-hook 'org-shiftup-final-hook 'windmove-up)
		       (add-hook 'org-shiftleft-final-hook 'windmove-left)
		       (add-hook 'org-shiftdown-final-hook 'windmove-down)
		       (add-hook 'org-shiftright-final-hook 'windmove-right)


		       )) ; end org use-package


; Custom Functions
(defun filter (condp lst)
  (delq nil
	(mapcar (lambda (x) (and (funcall condp x) x)) lst)))


(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))


; General settings
(setq inhibit-startup-message t)
(show-paren-mode t)
(column-number-mode t)
(global-auto-revert-mode t)
(setq indent-tabs-mode nil)
(put 'downcase-region 'disabled nil)
(fset 'yes-or-no-p 'y-or-n-p)


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

; follow mouse
(setq mouse-autoselect-window t)

; Browser Support
(setq browse-url-browser-function 'browse-url-generic
      browse-url-generic-program "chromium")


; Load theme files
(if (<= (display-color-cells) 256)
    (load-theme 'sanityinc-tomorrow-eighties t)
  (load-theme 'wombat t))


(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes (quote ("06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(ido-default-buffer-method (quote selected-window))
 '(safe-local-variable-values (quote ((org-clock-into-drawer . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

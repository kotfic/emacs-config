
(defvar emacs-config-dir (expand-file-name "~/.emacs.d"))
(defvar emacs-tmp-dir    (expand-file-name (concat emacs-config-dir "/" "tmp")))

;; Add subdirectories in emacs-config-dir
(let ((default-directory emacs-config-dir))
  (normal-top-level-add-subdirs-to-load-path))


;; adding package information
(require 'package)

(setq package-user-dir (concat emacs-config-dir "/" "packages"))
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
			 ("melpa" .  "http://melpa.milkbox.net/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
			 ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

; TODO - Need to figure out how to download org from elpa
; see (mapcar 'string-to-number (split-string (org-version) "[.]")
; get check for package description stuff to see if elpa version
; is higher than installed version

(let ((default-directory (concat emacs-config-dir "/" "lib")))
  (normal-top-level-add-subdirs-to-load-path))

(require 'use-package)
(setq use-package-verbose t)

;; Taken from danlamanna
(defmacro use-package-ensure(package &rest body)
  (declare (indent 1))
  `(use-package ,package
     :ensure t
     ,@body))




(use-package local_configs
  :demand t)


(use-package tern_dan
  :demand t)

(use-package-ensure scala-mode2
  :config
  (use-package-ensure ensime)
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))

(use-package-ensure s
  :defer 1)

(use-package-ensure dash
  :defer 1)

(use-package-ensure request
  :defer 1)

(use-package-ensure weechat
  :defer 1)

(use-package-ensure pdf-tools
  :defer 1
  :config 
  (pdf-tools-install)
  (setq-default pdf-view-display-size 'fit-page)
  (use-package org-pdfview
    :ensure t))


(use-package-ensure sauron
  :ensure t
  :defer 1
  :config
  (setq sauron-max-line-length nil
	sauron-hide-mode-line t
	))

(use-package-ensure guide-key
  :ensure guide-key
  :config
  (setq guide-key/guide-key-sequence '("C-c p" "C-c h"))
  (guide-key-mode 1))

(use-package-ensure helm
  :commands helm-mode
  :bind  (("C-h a" . helm-apropos)
	  ("C-c h g" . helm-google-suggest)
	  ("C-c h o" . helm-occur)
	  ("C-c h SPC" . helm-all-mark-rings)
	  ("M-x" . helm-M-x)
	  ("M-y" . helm-show-kill-ring)
	  ("C-x b" . helm-mini)
	  ("C-x C-f" . helm-find-files))
  :init
  (progn
    (helm-mode 1)
;  (global-set-key (kbd "C-c h") 'helm-command-prefix)
    (require 'helm-config)
  ;; The default "C-x c" is quite close to "C-x C-c", which quits Emacs.
  ;; Changed to "C-c h". Note: We must set "C-c h" globally, because we
  ;; cannot change `helm-command-prefix-key' once `helm-config' is loaded.

  ;(global-unset-key (kbd "C-x c"))

    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action) ; rebind tab to run persistent action
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action) ; make TAB works in terminal
    (define-key helm-map (kbd "C-z")  'helm-select-action) ; list actions using C-z
    
    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    (setq helm-split-window-in-side-p           t ; open helm buffer inside current window, not occupy whole other window
	  helm-move-to-line-cycle-in-source     t ; move to end or beginning of source when reaching top or bottom of source.
	  helm-ff-search-library-in-sexp        t ; search for library in `require' and `declare-function' sexp.
	  helm-scroll-amount                    8 ; scroll 8 lines other window using M-<next>/M-<prior>
	  helm-ff-file-name-history-use-recentf t)
    ))

(use-package-ensure projectile
  :config

  (projectile-global-mode t)

  (use-package helm-projectile
    :ensure helm-projectile
    :bind ("C-c p h" . helm-projectile))

  (helm-projectile-on))


;; (use-package notmuch
;;   :commands notmuch
;;   :init (progn
;; 	  (defun notmuch/tag (tag &optional nothread)
;; 	    "Tag regardless of mode"
;; 	    (cond ((eq major-mode 'notmuch-search-mode) 
;; 		   (notmuch-search-tag (list tag)))
;; 		  ((eq major-mode 'notmuch-show-mode)
;; 		   (notmuch-show-tag (list tag)))
;; 		  ((eq major-mode 'notmuch-tree-mode)
;; 		   (if (eq nothread t)
;; 		       (notmuch-tree-tag (list tag))
;; 		     (notmuch-tree-tag-thread (list tag))))))
;; 	  
;; 	  (defun notmuch/toggle-tag (tag &optional nothread)
;; 	    "Toggle tag regardless of mode, optional thread argument tags whole thread"
;; 	    (interactive "sTag:")
;; 	    ; if we tag it,  its not new
;; 	    (notmuch/tag "-review")
;; 	    (cond ((eq major-mode 'notmuch-search-mode) 
;; 		   (if (member tag (notmuch-search-get-tags))
;; 		       (notmuch-search-tag (list (concat "-" tag)))
;; 		     (notmuch-search-tag (list (concat "+" tag)))))
;; 		  ((eq major-mode 'notmuch-show-mode)
;; 		   (if (member tag (notmuch-show-get-tags))
;; 		       (notmuch-show-tag (list (concat "-" tag)))
;; 		     (notmuch-show-tag (list (concat "+" tag)))))
;; 		  ((eq major-mode 'notmuch-tree-mode)
;; 		   (if (eq nothread t)
;; 		       (if (member tag (notmuch-tree-get-tags))
;; 			   (notmuch-tree-tag (list (concat "-" tag)))
;; 			 (notmuch-tree-tag (list (concat "+" tag))))
;; 		     (if (member tag (notmuch-tree-get-tags))
;; 			 (notmuch-tree-tag-thread (list (concat "-" tag)))
;; 		       (notmuch-tree-tag-thread (list (concat "+" tag)))))
;; 		   )))
;; 
;; 	  (defun notmuch/prev-message ()
;; 	    (interactive)
;; 	    (cond ((eq major-mode 'notmuch-tree-mode)
;; 		   (notmuch-tree-prev-matching-message))))
;; 		   
;; 	  
;; 	  (defun notmuch/next-message ()
;; 	    (interactive)
;; 	    (cond ((eq major-mode 'notmuch-tree-mode)
;; 		   (notmuch-tree-next-matching-message))))
;; 		  
;; 	  )
;;   :config (progn
;; 	    (setq notmuch-saved-searches
;; 		  '((:name "review" :query "tag:review and not tag:delete and not tag:junk" :key "r" :sort-order 'newest-first)
;; 		    (:name "albany inbox" :key "a"
;; 			   :query "tag:inbox and tag:ualbany and not tag:delete and not tag:junk and not tag:draft" 
;; 			   :count-query "tag:inbox and tag:ualbany and not tag:delete and not tag:junk and not tag:draft"
;; 			   :sort-order 'newest-first)
;; 		    (:name "gmail inbox" :query "tag:inbox and tag:gmail" :key "g" :sort-order 'newest-first)
;; 		    (:name "unread" :query "tag:unread" :key "u" :sort-order 'newest-first)
;; 		    (:name "flagged" :query "tag:flagged" :key "f" :sort-order 'newest-first)
;; 		    (:name "sent" :query "tag:sent" :key "t" :sort-order 'newest-first)
;; 		    (:name "drafts" :query "tag:draft" :key "d" :sort-order 'newest-first)
;; 		    (:name "all mail" :query "*" :key "*" :sort-order 'newest-first)))
;; 		    
;; 
;; 	    
;; 	    (setq notmuch-crypto-process-mime t)
;; 
;; 	    (define-key notmuch-show-mode-map "g" #'notmuch-refresh-this-buffer)
;; 	    (define-key notmuch-search-mode-map "g" #'notmuch-refresh-this-buffer)
;; 
;; 	    (defhydra notmuch/hydra-list-tagger
;; 	      (:color blue
;; 		      :post notmuch/hydra-tagger/body)
;; 	      "notmuch-list-tagger"
;; 	      ("l" (notmuch/toggle-tag "list") "list")
;; 	      ("c" (notmuch/toggle-tag "csail") "csail")	      
;; 	      ("p" (notmuch/toggle-tag "phd") "phd")
;; 	      ("u" (notmuch/toggle-tag "umls") "umls")
;; 	      ("g" (notmuch/toggle-tag "general") "general")
;; 	      ("m" (notmuch/toggle-tag "mozlab") "mozlab")
;; 	      ("i" (notmuch/toggle-tag "ipython") "ipython")
;; 	      ("q" nil "quit"))
;; 	    
;; 	    (defhydra notmuch/hydra-research-tagger
;; 	      (:color blue
;; 		      :post notmuch/hydra-tagger/body)
;; 	      "notmuch-research-tagger"
;; 	      ("r" (notmuch/toggle-tag "research") "research")
;; 	      ("p" (notmuch/toggle-tag "projpet") "projpet")
;; 	      ("i" (notmuch/toggle-tag "i2b2") "i2b2")
;; 	      ("d" (notmuch/toggle-tag "dissertation") "dissertation")
;; 	      ("g" (notmuch/toggle-tag "general") "general")
;; 	      ("q" nil "quit"))
;; 
;; 	    (let* ((ht (defhydra notmuch/hydra-tagger
;; 			 (:color pink)
;; 			 "notmuch-tagger"
;; 			 ("l" (progn
;; 				(notmuch/toggle-tag "list")
;; 				(notmuch/hydra-list-tagger/body)) "list" :color blue)
;; 			 ("r" (progn
;; 				(notmuch/toggle-tag "research")
;; 				(notmuch/hydra-research-tagger/body)) "research" :color blue)
;; 
;; 			 ("cl" (notmuch/toggle-tag "class") "class")
;; 			 ("co" (notmuch/toggle-tag "consulting") "consulting")
;; 			 
;; 			 ("u" (notmuch/toggle-tag "unread") "unread")
;; 			 ("i" (notmuch/toggle-tag "inbox") "inbox")
;; 			 ("j" (notmuch/toggle-tag "junk") "junk")
;; 			 ("d" (notmuch/toggle-tag "delete") "delete")
;; 			 ("s" (notmuch/toggle-tag "star") "star")
;; 			 ("a" (notmuch/toggle-tag "admin") "admin")
;; 			 ("q" nil "quit"))))
;; 	      (define-key notmuch-search-mode-map (kbd "C-t") ht)
;; 	      (define-key notmuch-tree-mode-map (kbd "C-t") ht))
;; 	    
;;   ))

; IDO
;; (use-package ido
;; 	     :init (progn
;; 		     (ido-mode 'both) ; for buffers and files
;; 		     (setq
;; 		      ido-save-directory-list-file "~/.emacs.d/cache/ido.last"
;; 		      ido-ignore-buffers '("\\` " "^\*Mess" "^\*Back" ".*Completion" "^\*Ido" "^\*trace" "^\*compilation" "^\*GTAGS" "^session\.*" "^\*")
;; 		      ido-work-directory-list '("~/" "~/Desktop" "~/Documents" "~src")
;; 		      ido-case-fold  t                 ; be case-insensitive
;; 		      ido-enable-last-directory-history t ; remember last used dirs
;; 		      ido-max-work-directory-list 30   ; should be enough
;; 		      ido-max-work-file-list      50   ; remember many
;; 		      ido-use-filename-at-point nil    ; don't use filename at point (annoying)
;; 		      ido-use-url-at-point nil         ; don't use url at point (annoying)
;; 		      ido-max-prospects 10             ; don't spam my minibuffer
;; 		      ido-confirm-unique-completion nil)
;; 
;;                                         ; when using ido, the confirmation is rather annoying...
;; 		     (setq confirm-nonexistent-file-or-buffer nil)
;; 		     (fset 'dired 'ido-dired)
;; 		     
;; 		     ))




;IDO Ubiquitous
; TODO - need to figure out some of the org related configurations
;;(use-package ido-ubiquitous
;;	     :init (progn
;;		     (ido-ubiquitous-mode 1)))
	     

; Windmove
(use-package-ensure windmove
  :config
  (windmove-default-keybindings 'shift))

; Tramp
(use-package-ensure tramp
  :config
  (setq tramp-default-method "ssh"))

; Magit
(use-package-ensure magit
  :bind (("C-x g" . magit-status)))


; Twittering Mode
;; (use-package twittering-mode
;;   :commands (twit twittering-node) 
;;   :config (progn
;; 	    (setq twittering-use-master-password t
;; 		  twittering-cert-file "/etc/ssl/certs/ca-certificates.crt")))

; LaTeX

		     
; Auto complete
;(use-package auto-complete
;	     :config
;	     (setq
;	      ac-auto-start 2
;	      ac-override-local-map nil
;	      ac-use-menu-map t
;	      ac-candidate-limit 20))



; Jedi
; TODO Jedi goto-definition is not working.
(use-package-ensure jedi
  :defer t
  :bind (("C-c d" . jedi:show-doc)
	 ("M-SPC" . jedi:complete)
	 ("M-." . jedi:goto-definition))
  :init
  (defun pp:custom-jedi-setup ()
    (jedi:setup)
    (jedi:ac-setup))
  
  :config (progn
	    
	    (setq jedi:server-command
		  `("python2" ,(concat jedi:source-dir "jediepcserver.py")))
	    
	    (setq jedi:setup-keys t
		  jedi:tooltip-method nil
		  jedi:get-in-function-call-delay 300
		  jedi:complete-on-dot t)))


; Python
(use-package-ensure python
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

(use-package-ensure virtualenvwrapper
  :commands venv-workon
  :config
  (progn
    (venv-initialize-interactive-shells)
    (setq venv-location (expand-file-name "~/.venvs/"))
    ))

		       
; Prodigy
(use-package-ensure prodigy
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

	    (load "prodigy_laptop.el")
`	    (load "prodigy_work.el")
	    
	    ))

; Yasnippet

(use-package-ensure yasnippet
;  :init (progn
;	  (yas-global-mode 1))
  :config (progn
	    (setq yas-snippet-dirs '("~/.emacs.d/custom-snippets" 
				     "~/.emacs.d/packages/yasnippet-20150212.240/snippets"))


	    ; This could probably be more sophisticated
	    (defun preview-fragment ()
	      (if (looking-back "\) ")
		  (org-preview-latex-fragment)))

	    (add-hook 'yas-after-exit-snippet-hook 'preview-fragment)
	    (setq yas-triggers-in-field t)))



;(use-package  slime
;  :config (progn 
;	    (use-package slime-repl)
;	    (setq inferior-lisp-program "/usr/bin/sbcl")))



(use-package-ensure skewer-mode
  :defer 1
  :config (progn
	    (skewer-setup)))



					; Note,  should change this to try and auto-detect sbcl

(use-package-ensure octave
  :commands run-octave
  :defer t
  :mode (("\\.m\\'" . octave-mode))
  :config (progn
	    (setq inferior-octave-prompt ">> ")))


; Doc View Mode
(use-package-ensure doc-view
  :mode    (("\\.docx\\'" . doc-view-mode)
	    ("\\.odt\\'" . doc-view-mode))

  :config (progn
	    (setq doc-view-continuous t)
	    (setq doc-view-resolution 300)
	    (defun doc-view-rotate-current-page ()
	      "Rotate the current page by 90 degrees. Requires ImageMagick installation"
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

(use-package-ensure hydra
  :init
  (progn
    (global-set-key
     (kbd "<f2>")
     (defhydra toggle ()
       "zoom"
       ("i" text-scale-increase "in")
       ("o" text-scale-decrease "out"))
     )    
    ))

;(use-package paradox
;  :commands paradox-list-packages)



;; (use-package hackernews
;;   :commands hackernews
;;   :config (progn
;; 	    (setq hackernews-top-story-limit 50)
;; 	     )
;;   )

(use-package-ensure noflet)

(use-package-ensure elfeed-org
  :commands elfeed-org
  :config (progn
	    (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))))

(use-package-ensure elfeed
  :bind (("C-x w" . elfeed))
  :config (progn
	    (elfeed-org)
	    
	    (defun elfeed-www ()
	      (interactive)

	      (let ((link (elfeed-entry-link elfeed-show-entry)))
		(when link
		  (message "Sent to browser: %s" link)
		  (eww (elfeed-entry-link elfeed-show-entry)))))

	    (define-key elfeed-show-mode-map "w" #'elfeed-www)
	    
	    (setq-default elfeed-search-filter "@6-week-ago +unread -junk -test")
	    
	    )
  )

; Org Mode
(use-package-ensure org
  :commands org-mode
  :bind (("C-c l" . org-store-link)
	 ("C-c c" . org-capture)
	 ("C-c C-x C-o" . org-clock-out)
	 ("C-c a" . org-agenda)
	 ("C-c b" . org-iswitchb))
  
  :config
  (progn
    (defun org-agenda-schedule-time ()
      (let* ((marker (org-get-at-bol 'org-hd-marker))
	     (buffer (marker-buffer marker))
	     (pos (marker-position marker)))
	(switch-to-buffer buffer)
	(goto-char pos))
      (goto-char (org-element-property :end (org-element-at-point))))
    
    (defun org-claws-link-command (pth)
      (shell-command (concat "claws-mail --select " pth))
      (shell-command "i3-msg -q \'[class=\"Claws-mail\"]\' focus")
      (message pth))
    
    
    (defun org/protocol-capture-p ()
      "Return true if this capture was initiated via org-protocol."
      (equal (frame-parameter (selected-frame) 'name) "Floating Capture"))
    
    (defun org/capture-after-finalize ()
      "Delete frame if capture was initiated via org-protocol"
      (if (org/protocol-capture-p) (delete-frame)))
    
    (defun protocol-org-capture (note)
      "Generally allow capture"
      (interactive)
      (make-frame '((name . "Floating Capture")))
      (select-frame-by-name "Floating Capture")
      
      (noflet ((org-switch-to-buffer-other-window (&rest args)
						  (org-no-popups
						   (apply 'switch-to-buffer args))))
	(let ((org-capture-link-is-already-stored t))
	  (org-store-link-props :annotation note)
	  
	  (condition-case err
	      (org-capture)
	    (error (delete-frame))))))
    
    
    (defun org-datetree/internal_to_datetree_date(internal-time)
      
      (let* ((ts (decode-time internal-time))
	     (SEC (nth 0 ts))
	     (MINUTE (nth 1 ts))
	     (HOUR (nth 2 ts))
	     (DAY (nth 3 ts))
	     (MONTH (nth 4 ts))
	     (YEAR (nth 5 ts)))
	(list MONTH DAY YEAR)))
    
    
    (defun org-datetree/refile (file time)
      (interactive)  
      (save-excursion 
	(org-back-to-heading)
	(let ((file_buf (or (find-buffer-visiting file)
			    (find-file-noselect file)))
	      (date  (org-datetree/internal_to_datetree_date  time)))
					; cut the subtree
	  (org-cut-subtree)
	  
					; paste it into the datetree buffer
	  (with-current-buffer file_buf
	    (org-datetree-find-date-create date)
	    (let ((level (org-get-valid-level (funcall outline-level) 1)))
	      (org-end-of-subtree t t)
	      (org-back-over-empty-lines)
	      (org-paste-subtree level))))))
    
    
    (defun org/start-heading-text ()			
      (save-excursion
	(org-back-to-heading)			 
	(re-search-forward 
	 (plist-get (cadr (org-element-at-point)) :title))
	(match-beginning 0)))
    
    (defun org/refile-clocked-to-journal ()
      (interactive)
      (save-excursion
	(org-back-to-heading)
	(let ((file "~/org2/journal.org")
	      (tags 
	       (org-icompleting-read "Tags: "
				     'org-tags-completion-function
				     nil nil nil 'org-tags-history))
	      (internal-time (org-time-string-to-time 
			      (org-entry-get (point) "CLOCK")))
	      (ts-point (org/start-heading-text)))
	  
					; set the tags
	  (org-set-tags-to tags)
	  
					; format the heading
	  (goto-char ts-point)
	  (org-insert-time-stamp internal-time t 'inactive "" " - ")
	  (org-datetree/refile file internal-time)
	  )))
    
    
    (defun yas/org-very-safe-expand ()
      (let ((yas/fallback-behavior 'return-nil)) (yas/expand)))
    
    (defun org-yas-conflict ()
      (make-variable-buffer-local 'yas/trigger-key)
      (setq yas/trigger-key [tab])
      (add-to-list 'org-tab-first-hook 'yas/org-very-safe-expand)
      (define-key yas/keymap [tab] 'yas/next-field-or-maybe-expand))
    


    (setq org-log-done 'time
	  org-use-tag-inheritance nil
	  org-hide-leading-stars t
	  org-startup-indented t
	  org-export-backends '(ascii html icalendar latex md odt)
	  org-startup-with-inline-images "inlineimages"
	  org-time-clocksum-format '(:hours "%d" :require-hours t :minutes ":%02d" :require-minutes t)
	  org-format-latex-options '(:foreground default 
						 :background default 
						 :scale 1.4 
						 :html-foreground "Black" 
						 :html-background "Transparent" 
						 :html-scale 1.0 
						 :matchers ("begin" "$1" "$" "$$" "\\(" "\\["))

	  org-agenda-files (concat emacs-config-dir "/.agenda-files" )
	  org-agenda-todo-ignore-with-date t
	  org-agenda-tags-todo-honor-ignore-options t
	  org-agenda-skip-deadline-prewarning-if-scheduled t
	  org-default-notes-file (concat org-directory "/unfiled.org")
	  org-refile-targets '((org-agenda-files . (:maxlevel . 6)))
	  org-refile-use-outline-path 'file
	  org-outline-path-complete-in-steps nil
	  org-export-odt-preferred-output-format "docx"
	  org-export-odt-styles-file nil
	  org-file-apps '((auto-mode . emacs)
			  ("\\.mm\\'" . default)
			  ("\\.x?html?\\'" . default))
    
	  org-babel-use-quick-and-dirty-noweb-expansion t
	  org-latex-pdf-process  '("latexmk -c"
				   "pdflatex -interaction nonstopmode -output-directory %o %f" 
				   "biber --trace %b.bcf"
				   "pdflatex -interaction nonstopmode -output-directory %o %f")
        
	  org-src-preserve-indentation t
	  org-src-fontify-natively t
	  org-confirm-babel-evaluate nil)
    

    (setq org-agenda-custom-commands '(("n" "Agenda and TODO's"
					((agenda "")
					 (tags-todo "+today")
					 (tags-todo "+week")
					 (tags-todo "+dissertation")
					 (tags-todo "+current")
					 ))
				       ("P" "Process Improvements"
					((tags-todo "CATEGORY=\"Process\"-backburner")
					 (tags-todo "CATEGORY=\"Process\"+backburner")
					 ))
				       ))
    
    
    
    (setq org-capture-templates
	  '(("t" "TODO" entry
	     (file+headline "~/org2/unfiled.org" "Tasks")
	     "* TODO %? :current:\n  %i\n  %a")
	    ("s" "Schedule" entry (file+headline "~/org2/unfiled.org" "Tasks")
	     "* %? \n  %i\n  %a")
	    ("e" "Email" entry 
	     (file+headline "~/org2/unfiled.org" "Email")
	     "* EMAIL %? :current:\n %i\n %a ")
	    ("m" "Meeting" entry 
	     (file+headline "~/org2/unfiled.org" "Meetings") 
	     "* %?\n  %i\n  %a")
	    ("c" "Clock in" entry 
	     (file+datetree "~/org2/journal.org")
	     "* %U - %? %^g\n %i\n %a" 
	     :clock-in t :clock-keep t)
	    ))
    


    ; Additional packages and configurations
    
    (use-package org-compat :if window-system)
    (use-package-ensure ox-reveal)
    
    (use-package org-id
      :config (progn
		(setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
		      org-link-to-org-use-id 'create-if-interactive-and-no-custom-id)))
    
    
    (use-package org-datetree)
    
    (use-package org-crypt
      :config (progn
		(org-crypt-use-before-save-magic)
		(setq org-tags-exclude-from-inheritance (quote ("crypt")))
		;; GPG key to use for encryption
		;; Either the Key ID or set to nil to use symmetric encryption.
		(setq org-crypt-key "E4CAD065")))
    
					; Org Bullets
    (use-package-ensure org-bullets :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))
    
    
    
    (use-package org-protocol
      :config
      (progn
	(add-hook 'org-capture-after-finalize-hook 'org/capture-after-finalize)
	
	(add-to-list 'org-protocol-protocol-alist
		     '("Generic Org Capture"
		       :protocol "org-capture"
		       :function protocol-org-capture))))
    


    ; Misc Commands etc
    (org-add-link-type "claws" 'org-claws-link-command)    
                      
    (org-babel-do-load-languages
     'org-babel-load-languages
     '((emacs-lisp . t)
       (python . t)
       (R . t)
       (html . t)
       (latex . t)
       (sh . t )
       (sql . t)
       (org . t)
       (ditaa . t)
       (dot . t)))
    

    

    ;(define-key global-map "\C-cr" 'org-capture)

    ; Hooks

    (add-hook 'org-mode-hook (lambda () (org-yas-conflict)))    
    (add-hook 'org-shiftup-final-hook 'windmove-up)
    (add-hook 'org-shiftleft-final-hook 'windmove-left)
    (add-hook 'org-shiftdown-final-hook 'windmove-down)
    (add-hook 'org-shiftright-final-hook 'windmove-right)
    
    (load "org-meta.el")
    
    )) ; end org use-package




; Add 'zip' files to dired 'Z'
(eval-after-load "dired-aux"
   '(add-to-list 'dired-compress-file-suffixes 
                 '("\\.zip\\'" ".zip" "unzip")))

(defun on-ec ()
  "run after launching via 'ec' script"
    (set-frame-parameter (selected-frame) 'alpha 0.9))


; Custom Functions
;(defun reload-browser ()
;  (interactive)
;  (shell-command (concat
;		  "CURRENT=$(xdotool getwindowfocus);"
;		  "xdotool search --onlyvisible --name chromium windowfocus key 'ctrl+r';"
;		  "xdotool windowactivate $CURRENT")))



(defun filter (condp lst)
  (delq nil
	(mapcar (lambda (x) (and (funcall condp x) x)) lst)))


(defun transparency (value)
  "Sets the transparency of the frame window. 0=transparent/100=opaque"
  (interactive "nTransparency Value 0 - 100 opaque:")
  (set-frame-parameter (selected-frame) 'alpha value))


(defun antiword-buffer ()
  "Takes the current buffer as input to the external program antiword.

If the current buffer is a ms-word document it's contents are replaced
with the output from antiword and the extension `.doc' is replaced
with `.txt' in the buffer-file-name."
  (interactive)
  (let ((txt-buffer-file-name (concat (substring (buffer-file-name) 0 -4)
                                      ".txt")))
    (shell-command-on-region (point-min) (point-max)
                             "cat | antiword -" nil t nil)
    (undo-start)
    (if (equal (buffer-string) "- is not a Word Document.\n")
        (or (undo-more 1)
            (message "%s - is not a Word Document."(current-buffer)))
      (set-visited-file-name txt-buffer-file-name)
      (not-modified))))


(defun toggle-window-split ()
  (interactive)
  (if (= (count-windows) 2)
      (let* ((this-win-buffer (window-buffer))
	     (next-win-buffer (window-buffer (next-window)))
	     (this-win-edges (window-edges (selected-window)))
	     (next-win-edges (window-edges (next-window)))
	     (this-win-2nd (not (and (<= (car this-win-edges)
					 (car next-win-edges))
				     (<= (cadr this-win-edges)
					 (cadr next-win-edges)))))
	     (splitter
	      (if (= (car this-win-edges)
		     (car (window-edges (next-window))))
		  'split-window-horizontally
		'split-window-vertically)))
	(delete-other-windows)
	(let ((first-win (selected-window)))
	  (funcall splitter)
	  (if this-win-2nd (other-window 1))
	  (set-window-buffer (selected-window) this-win-buffer)
	  (set-window-buffer (next-window) next-win-buffer)
	  (select-window first-win)
	  (if this-win-2nd (other-window 1))))))


(global-set-key (kbd "C-c |") 'toggle-window-split)


;;;;
(setq ibuffer-saved-filter-groups
      (quote (("default"
	       ("org" (or (mode . org-mode)
			  (mode ."^\\*Org.*$")))
	       
	       ("python" (mode . python-mode))
	       
	       ("erc" (mode . erc-mode))
	       ("mail" (or
			(mode . notmuch-show-mode)
			(mode . notmuch-search-mode)
			(mode . notmuch-hello)
			(mode . notmuch-tree-mode)))
	       ("elfeed" (name . "^\\*elfeed.*"))
	       ("emacs" (or
			 (name . "^\\*scratch\\*$")
			 (name . "^\\*Messages\\*$")
			 (mode . lisp-mode)))
	       
	       ("dired" (mode . dired-mode))
	       
	       ))))

(add-hook 'ibuffer-mode-hook
	  (lambda ()
	    (ibuffer-switch-to-saved-filter-groups "default")))


(global-set-key (kbd "C-x C-b") 'ibuffer)
;;;;

(setq auto-mode-alist
      (append '(("\\.doc\\'" . antiword-buffer))
              auto-mode-alist))


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

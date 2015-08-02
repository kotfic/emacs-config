;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;     NOT MUCH
;;;;;;;;;;;;;;;;;;;;;;;;;;
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


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;     IDO
;;;;;;;;;;;;;;;;;;;;;;;;;;
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

;;;;;;;;;;;;;;;;
;;;;    IDO Ubiquitous
;;;;;;;;;;;;;;;;
; TODO - need to figure out some of the org related configurations
;;(use-package ido-ubiquitous
;;	     :init (progn
;;		     (ido-ubiquitous-mode 1)))
	     

;;;;;;;;;;;;;;;;
;;;;;    Twittering Mode
;;;;;;;;;;;;;;;;
;;
;; (use-package twittering-mode
;;   :commands (twit twittering-node) 
;;   :config (progn
;; 	    (setq twittering-use-master-password t
;; 		  twittering-cert-file "/etc/ssl/certs/ca-certificates.crt")))



;;;;;;;;;;;;;;;;;
;;;;;    Auto complete
;;;;;;;;;;;;;;;;;
;;
;; (use-package auto-complete
;;	     :config
;;	     (setq
;;	      ac-auto-start 2
;;	      ac-override-local-map nil
;;	      ac-use-menu-map t
;;	      ac-candidate-limit 20))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Paradox
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package paradox
;;  :commands paradox-list-packages)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Hackernews
;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package hackernews
;;   :commands hackernews
;;   :config (progn
;; 	    (setq hackernews-top-story-limit 50)
;; 	     )
;;   )



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Slime
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package  slime
;;  :config (progn 
;;	    (use-package slime-repl)
;;	    (setq inferior-lisp-program "/usr/bin/sbcl")))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Skewer
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package-ensure skewer-mode
;;  :defer 1
;;  :config (progn
;;	    (skewer-setup)))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Octave
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; (use-package-ensure octave
;;  :commands run-octave
;;  :defer t
;;  :mode (("\\.m\\'" . octave-mode))
;;  :config (progn
;;	    (setq inferior-octave-prompt ">> ")))


;; (use-package-ensure noflet)


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Guide Key
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; (use-package-ensure guide-key
;;   :defer 1
;;   :config
;;   (setq guide-key/guide-key-sequence '("C-c p" "C-c h"))
;;   (guide-key-mode 1))
;; 

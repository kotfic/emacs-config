(defvar emacs-config-dir (expand-file-name "~/.emacs.d"))

(defvar emacs-tmp-dir    (expand-file-name (concat emacs-config-dir "/" "tmp")))

;; adding package information
(require 'package)

(setq package-user-dir (concat emacs-config-dir "/" "packages"))
(setq package-enable-at-startup nil)
(setq package-archives '(("gnu" . "http://elpa.gnu.org/packages/")
                         ("melpa" .  "https://melpa.org/packages/")
                         ("marmalade" . "http://marmalade-repo.org/packages/")
                         ("org" . "http://orgmode.org/elpa/")))
(package-initialize)

;; Bootstrap `use-package'
(unless (package-installed-p 'use-package)
  (package-refresh-contents)
  (package-install 'use-package))

(require 'diminish)
(require 'bind-key)


;(let* ((my-lisp-dir "~/.emacs.d/lib/")
;       (default-directory my-lisp-dir)
;       (orig-load-path load-path))
;  (setq load-path (cons my-lisp-dir nil))
;  (normal-top-level-add-subdirs-to-load-path)
;  (nconc load-path orig-load-path))

(require 'use-package)
;(setq use-package-verbose t)

;; Taken from danlamanna
(setq use-package-always-ensure t)

(use-package powerline)
(use-package badger-theme)


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Utilities
;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package dash)

(use-package noflet)

(use-package s)

(use-package request)


(use-package hydra
  :config
  (progn
    (global-set-key
     (kbd "<f2>")
     (defhydra toggle ()
       "zoom"
       ("i" text-scale-increase "in")
       ("o" text-scale-decrease "out"))
     )
    ))

;; Notmuch mail client
(when (require 'notmuch nil 'noerror)
  (setq notmuch-search-oldest-first nil )

  (defmacro notmuch/tree_tag (&rest tags)
    `(lambda ()
       (interactive)
       (notmuch-tree-tag-thread (list ,@tags))))

  (defmacro notmuch/search_tag (&rest tags)
    `(lambda ()
       (interactive)
       (notmuch-search-tag (list ,@tags))))


  (defmacro notmuch/show_tag (&rest tags)
    `(lambda ()
       (interactive)
       (notmuch-show-tag (list ,@tags))))


  ;; Any thread with +ignore as a tag should ignore all new emails
  ;; see mail/.notmuch/hooks/post-new

  (define-key notmuch-tree-mode-map "e" (notmuch/tree_tag "-unread" "-review" "-inbox" "-important"))
  (define-key notmuch-tree-mode-map "j" (notmuch/tree_tag "+junk" "-unread" "-inbox" "-importaint"))
  (define-key notmuch-tree-mode-map "i" (notmuch/tree_tag "+ignore" "-inbox" "-important"))

  (define-key notmuch-search-mode-map "e" (notmuch/search_tag "-unread" "-review" "-inbox" "-important"))
  (define-key notmuch-search-mode-map "j" (notmuch/search_tag "+junk" "-unread" "-inbox" "-importaint"))
  (define-key notmuch-search-mode-map "i" (notmuch/search_tag "+ignore" "-inbox" "-important"))

  (define-key notmuch-show-mode-map "e" (notmuch/show_tag "-unread" "-review" "-inbox" "-important"))
  (define-key notmuch-show-mode-map "j" (notmuch/show_tag "+junk" "-unread" "-inbox" "-importaint"))
  (define-key notmuch-show-mode-map "i" (notmuch/show_tag "+ignore" "-inbox" "-important"))



  (defmacro notmuch/query_command (query)
    `(lambda ()
       (interactive)
       (notmuch-search ,query)))

  (global-set-key (kbd "C-c m") (defhydra hydra-mail (:hint nil :color blue :idle 1.0)
        "
                                        ╭──────┐
                                        │ Mail │
  ╭─────────────────────────────────────┴──────╯
    [_k_] Kitware                [_c_] Custom
    [_g_] Gmail
    [_a_] Albany
  ---------------------------------------------
        "
        ("k" (notmuch-search "path:kitware/** and tag:inbox and date:90d.."))
        ("g" (notmuch-search "path:gmail/** and date:90d.."))
        ("a" (notmuch-search "path:ualbany/** and date:90d.."))
        ("c" (notmuch-search))
        ))




  (defun notmuch/first_unread_or_last (thread_id)
    (let ((unread (notmuch-call-notmuch-sexp
                   "search" "--output=messages" "--sort=oldest-first" "--format=sexp" (concat thread_id " and tag:unread")))
          (thread (notmuch-call-notmuch-sexp
                   "search" "--output=messages" "--sort=newest-first" "--format=sexp" thread_id)))
      (concat "id:" (if unread
                        (car unread)
                      (car thread)))))

  ;; Show a tree view when we select from search
  (defun notmuch/search-show-tree-thread (tmp)
    "Display the currently selected thread as a tree."
    (interactive "P")
    (let ((thread-id (notmuch-search-find-thread-id)))
      (if (> (length thread-id) 0)
          (notmuch-tree thread-id nil (notmuch/first_unread_or_last thread-id) nil t)
        (message "Not on thread!"))))

  (define-key notmuch-search-mode-map (kbd "<C-return>") 'notmuch/search-show-tree-thread)

  (define-key notmuch-search-mode-map (kbd "RET") 'notmuch-search-show-thread)

  ;; TODO: Write notmuch/*_tag macros for other groups
  ;; TODO: Write single unified macro so we can have one list for all modes
  ;; TODO:

  (define-key notmuch-show-mode-map "U" 'browse-url-at-point)

  ;; Handle MSMTP account by passing correct account flag ('-a')
  ;; https://www.emacswiki.org/emacs/GnusMSMTP#toc2

  (setq message-send-mail-function 'message-send-mail-with-sendmail)
  (setq sendmail-program "/usr/sbin/msmtp")
  (defun cg-feed-msmtp ()
    (if (message-mail-p)
        (save-excursion
          (let* ((from
                  (save-restriction
                    (message-narrow-to-headers)
                    (message-fetch-field "from")))
                 (account
                  (cond
                   ;; I use email address as account label in ~/.msmtprc
                   ((string-match "chris.kotfila@kitware.com" from) "kitware")
                   ((string-match "kotfic@gmail.com" from) "gmail")
                   ((string-match "ckotfila@albany.edu" from) "ualbany")
                   )))

            (setq message-sendmail-extra-arguments (list '"-a" account)))))) ; the original form of this script did not have the ' before "a" which causes a very difficult to track bug --frozencemetery
  (setq message-sendmail-envelope-from 'header)
  (add-hook 'message-send-mail-hook 'cg-feed-msmtp)
  (setq notmuch-fcc-dirs nil)

  ;; Split horizontally instead of vertically when creting a new message
  (defadvice notmuch-tree-show-message-in
      (around kotfic-use-horizontal-rather-than-vertical activate)
    (cl-letf (((symbol-function 'split-window-vertically)
               #'(lambda (&optional SIZE) (split-window-horizontally))))  ad-do-it))

  (require 'org-notmuch)
  )

(use-package avy
  :ensure t)

(use-package link-hint
  :ensure t
  :bind
  ("C-c o" . link-hint-open-link)
  ("C-c O" . link-hint-copy-link))

;; flycheck
(use-package flycheck
  :diminish flycheck-mode
  :init
  (progn
    ;; Use this var to disable for some modes
    ;; (setq flycheck-global-modes t)
    (setq flycheck-temp-prefix (concat emacs-tmp-dir "/" "flycheck/flycheck"))
    (add-hook 'after-init-hook #'global-flycheck-mode)
    ))


; Tramp
(use-package tramp
  :defer 1
  :config
  (setq tramp-default-method "ssh"
        password-cache-expiry 300)
  )

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;   Javascript etc
;;;;;;;;;;;;;;;;;;;;;;;;;;;


(use-package js2-mode
  ;; flycheck does checking for jshint and jscs
  :config (progn
            (custom-set-variables
             '(js2-mode-show-parse-errors nil)
             '(js2-strict-missing-semi-warning nil))

;            (add-hook 'js2-mode-hook 'js2-refactor-mode)
            (add-hook 'js2-mode-hook 'rainbow-mode)
            (add-hook 'js2-mode-hook 'company-mode)
            (add-hook 'js2-mode-hook 'tern-mode)
;            (add-hook 'js2-mode-hook 'flycheck-mode)

            ;; spaces not tabs
            (add-hook 'js2-mode-hook
                      '(lambda () (progn
                                    (set-variable 'indent-tabs-mode nil)))))
  :mode ("\\.js\\'" . js2-mode))


;; js2-refactor
;(use-package js2-refactor
;  :defer t)

;; rainbow-mode
(use-package rainbow-mode
  :defer t
  :pin gnu)

;; company
(use-package company
  :config
  (progn
    (use-package company-tern
      :config (progn
                (add-to-list 'company-backends 'company-tern)))))


;; tern
(use-package tern
  :defer t)


;; jade-mode
(use-package jade-mode
  :mode ("\\.jade\\'" . jade-mode))


;; stylus-mode
(use-package stylus-mode
  :mode ("\\.styl\\'" . stylus-mode)
  :config
  (progn
    (add-hook 'stylus-mode-hook 'rainbow-mode)))



;;;;;;;;;;;;;;;;;;;;;;
;;; Scala mode
;;;;;;;;;;;;;;;;;;;;;;

(use-package scala-mode2
  :mode ("\\.scala\\'" . scala-mode)
  :ensure t
  :config
  (use-package ensime :ensure t)

  (setq
   ensime-sbt-perform-on-save "compile")
  (add-hook 'scala-mode-hook 'ensime-scala-mode-hook))



;;;;;;;;;;;;;;;;;;;;;;;
;;;; Communication & Notifications
;;;;;;;;;;;;;;;;;;;;;;;


(use-package weechat
  :defer 1
  :config
  (setq weechat-auto-monitor-buffers t
        weechat-auto-close-buffers t
        weechat-auto-monitor-new-buffers 'silent)

  (defvar weechat/match-line-regex
    "^\\([0-9]+:[0-9]+:[0-9]+\\)\s\\([^\s]*\\):\s*\\(.*\\)")

  (defvar weechat/ignore-users '("kotfic"))

  ;; Get the last line of text from a buffer
  (defun buffer/last-line (buffer &optional num)
    (or num (setq num 1))
    (save-excursion
      (set-buffer buffer)
      (save-excursion
        (end-of-buffer)
        (forward-line (- 1 num))
        (backward-char)
        (let ((end (point)))
          (forward-line 0)
          (buffer-substring-no-properties (point) end)))))

  ;; Parse a weechat line into a structured p-list
  (defun weechat/parse-line (msg)
    (when (s-matches? weechat/match-line-regex msg)
      (let ((fields '(:raw :time :user :message))
            (values (s-match weechat/match-line-regex msg)))
        (apply #'append (mapcar* (lambda (a b) (list a b)) fields values)))))

  ;; What to do if
  (defun weechat/sauron-action (plst)
    (sauron-switch-to-marker-or-buffer (plist-get plst :marker)))


  (setq
  ;; Only add event if no events for last channel-insensitivity amount of time
   weechat/channel-insensitivity 60
   ;; Hash of last message times for each channel (by :short_name)
   weechat/channel-event-hash (make-hash-table :size 100 :test 'equal))


  ;; Return true if there has been no activity since weechat/channel-insensitivity
  (defun weechat/fresh-channel-event (channel)
    ;; we only store the lsb, which is good enough for 2^16 seconds.
    (let* ((now-lsb (float-time))
           (tstamp (gethash channel weechat/channel-event-hash)))

      ;; Always update channel hash with most recent event
      (puthash channel now-lsb weechat/channel-event-hash)

      (cond ((not tstamp) t)
            ((> (- now-lsb tstamp) weechat/channel-insensitivity) t)
            (t nil))))

  ;; Add an event at priority 3
  (defun weechat/sauron-add-event (msg prio)
    (when (not (member (plist-get msg :user) weechat/ignore-users))
      (let ((jump-pos (save-window-excursion
                        (switch-to-buffer (plist-get msg :emacs/buffer))
                        (point-max-marker))))
        (sauron-add-event 'weechat prio
                          (format "[%s] %s: %s"
                                  (plist-get msg :short_name)
                                  (plist-get msg :user)
                                  (plist-get msg :message))
                          (lexical-let ((plst (append msg `(:marker ,jump-pos))))
                            (lambda () (weechat/sauron-action plst)))
                          ))))


  (defun weechat/handle-message (buffer-ptr)
    (let ((raw-line  (buffer/last-line (weechat--emacs-buffer buffer-ptr)))
          (buffer-hash (weechat-buffer-hash buffer-ptr)))
      (when (s-matches? weechat/match-line-regex raw-line))
        (let ((msg (weechat/parse-line raw-line))
              (buffer-facts (list
                             :short_name (gethash "short_name" buffer-hash)
                             :emacs/buffer (gethash :emacs/buffer buffer-hash)))
              (buffer-local-facts (apply #'append
                                         (mapcar (lambda (x)
                                                   (list (make-symbol (concat ":" (car x))) (cdr x)))
                                                 (gethash "local_variables" buffer-hash))))
              (prio 2))

          (when (weechat/fresh-channel-event (plist-get buffer-facts :short-name))
            (incf prio))

          (weechat/sauron-add-event
           (append msg buffer-facts buffer-local-facts) prio))))

  (add-hook 'weechat-message-post-receive-functions
            'weechat/handle-message)

  (setq weechat-modules '(weechat-button weechat-complete weechat-speedbar))

  )

(use-package sauron
  :defer 1
  :config
  (setq sauron-max-line-length nil
        sauron-hide-mode-line t
        ;;      sauron-separate-frame nil
        sauron-watch-patterns '("kotfic"
                                "\\[Github\\]")
        )



  )


(use-package elfeed-org
  :defer 1
  :commands elfeed-org
  :config (progn
            (setq rmh-elfeed-org-files (list "~/.emacs.d/elfeed.org"))))

(use-package elfeed
  :defer 1
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


;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;   PDFs and DOCS
;;;;;;;;;;;;;;;;;;;;;;;;;

(when (executable-find "make")
  (use-package pdf-tools
    :defer 1
    :config
    (pdf-tools-install t t t)
    (setq-default pdf-view-display-size 'fit-page)
    (use-package org-pdfview
      :ensure t)))

; Doc View Mode
(use-package doc-view
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

(use-package yaml-mode
  :mode ("\\.yml$" . yaml-mode))

;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;; Project Management
;;;;;;;;;;;;;;;;;;;;;;;;;;;

(use-package helm
  :commands helm-mode
  :bind  (("C-h a" . helm-apropos)
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
    (use-package helm-ag)
    (use-package helm-dash
      :config
      (setq
       helm-dash-common-docsets '("Ansible" "Python 2" "Docker")
       helm-dash-browser-func 'eww)
      )

    (use-package helm-descbinds)

    ;; rebind tab to run persistent action
    (define-key helm-map (kbd "<tab>") 'helm-execute-persistent-action)
    ;; make TAB works in terminal
    (define-key helm-map (kbd "C-i") 'helm-execute-persistent-action)
    ;; list actions using C-z
    (define-key helm-map (kbd "C-z")  'helm-select-action)

    (when (executable-find "curl")
      (setq helm-google-suggest-use-curl-p t))

    ;; open helm buffer inside current window, not occupy whole other window
    (setq helm-split-window-in-side-p           t
          ;; move to end or beginning of source when reaching top or bottom of source.
          helm-move-to-line-cycle-in-source     t
          ;; search for library in `require' and `declare-function' sexp.
          helm-ff-search-library-in-sexp        t
          ;; scroll 8 lines other window using M-<next>/M-<prior>
          helm-scroll-amount                    8
          helm-ff-file-name-history-use-recentf t)

    (global-set-key (kbd "C-c h") (defhydra hydra-helm (:hint nil :color blue)
        "
                                                                          ╭──────┐
      File Search            Text Search         Misc                     │ Helm │
  ╭───────────────────────────────────────────────────────────────────────┴──────╯
      [_f_] Find Files         [_s_] Ag              [_a_] Apropos              [_r_] Resume
      [_l_] System Locate      [_i_] Semantic        [_k_] Kill Ring
      [_g_] Google Search      [_o_] Occur           [_t_] Helm Top
                             [_d_] Dash
  --------------------------------------------------------------------------------
        "
        ("f" helm-find-files)
        ("l" helm-locate)
        ("g" (lambda (s)
               (interactive (list (read-string "SearchFor: "
                                               nil 'helm-surfraw-input-history
                                               (thing-at-point 'symbol))))
               (helm-surfraw s "google")))


        ("s" helm-do-ag)
        ("i" helm-semantic-or-imenu)

        ("a" helm-apropos)
        ("k" helm-show-kill-ring)
        ("t" helm-top)
        ("o" helm-occur)
        ("r" helm-resume)
        ("d" helm-dash)

        ))

    ))

(use-package projectile
  :init
    (setq projectile-keymap-prefix (kbd "C-c C-p"))
  :config
  (projectile-global-mode t)

  (use-package helm-swoop)

  (use-package helm-projectile
    :ensure helm-projectile)

  (helm-projectile-on)

  (setq grep-find-ignored-files '()
        grep-find-ignored-directories '())

  (setq projectile-mode-line
      '(:eval
        (if (file-remote-p default-directory)
            " Projectile[*remote*]"
          (format " Projectile[%s]" (projectile-project-name)))))

  (setq projectile-file-exists-remote-cache-expire nil)

  (global-set-key (kbd "C-c p") (defhydra hydra-projectile (:hint nil :color blue)
                                  "
                                                                                                ╭────────────┐
   Files & Directories     Find & Replace         Buffer Opperations    Commands                │ Projectile │
  ╭─────────────────────────────────────────────────────────────────────────────────────────────┴────────────╯
   [_f_] Find File (this)    [_ss_] Ag Search         [_k_] Kill Buffers      [_c_] Run Async Command   [_p_] Switch Project
   [_F_] Find File (all)     [_sg_] Grep Search       [_S_] Save Buffers      [_C_] Run Command         [_h_] Helm Projectile
   [_e_] Recent Files        [_sa_] Ack Search        [_b_] Switch Buffer     [_t_] Test Project        [_q_] Quit
   [_d_] Find Directory      [_o_]  Multi-Occur       [_v_] Project VC
   [_D_] Goto Project Root   [_r_]  Query Replace
  -------------------------------------------------------------------------------------------------------------
        "

                                  ("f" helm-projectile-find-file)
                                  ("F" helm-projectile-find-file-in-known-projects)
                                  ("e" helm-projectile-recentf)
                                  ("d" helm-projectile-find-dir)
                                  ("D" projectile-dired)

                                  ;; TODO - need to be specifically configured for each project
                                  ;; C-c p c    Runs a standard compilation command for your type of project.
                                  ;; C-c p P    Runs a standard test command for your type of project.
                                  ("t" projectile-test-project)
                                  ;; C-c p t    Toggle between an implementation file and its test file.

                                  ("ss" helm-projectile-ag)
                                  ("sg" helm-projectile-grep)
                                  ("sa" helm-projectile-ack)
                                  ("o" helm-projectile-multi-occur)
                                  ("r" projectile-replace)

                                  ("k" projectile-kill-buffers)
                                  ("S" projectile-save-project-buffers)
                                  ("b" helm-projectile-switch-to-buffer)
                                  ("v" projectile-vc)

                                  ("C" projectile-run-shell-command-in-root)
                                  ("c" projectile-run-async-shell-command-in-root)

                                  ("p" helm-projectile-switch-project)

                                  ("h" helm-projectile)
                                  ("q" nil)

                                  ))

  (defadvice projectile-project-root (around ignore-remote first activate)
    (unless (file-remote-p default-directory) ad-do-it))

    )



; Prodigy
(use-package prodigy
  :defer 1
  :commands prodigy
  :bind (("<f12>" . prodigy))
  :config (progn
            ; (add-hook 'prodigy-mode-hook 'virtualenv-minor-mode)


            ; (load "prodigy_python.el")


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


            (prodigy-define-service
              :name "Geonotebook jupyter notebook"
              :command "jupyter"
              :cwd "/home/kotfic/src/jupyter/geonotebook/notebooks"
              :args '("notebook" "--no-browser")
              :tags '(jupyter)
              :init (lambda ()
                      (venv-workon "jupyter"))
              :stop-signal 'sigkill )



            ;(load "prodigy_laptop.el")
            ;(load "prodigy_work.el")

            ))

(use-package magit
  :defer 1
  :bind (("C-x g" . magit-status)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;; Python
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;



; Python
(use-package python
  :defer 1
  :commands python-mode
  :config (progn
            (setq python-shell-interpreter "ipython"
                  python-shell-interpreter-args "-i --simple-prompt --colors=Linux")

                                        ; auto pair
            ; (use-package autopair)

                       ; adding hooks
            (add-hook 'python-mode-hook (lambda ()
                                          (unless (tramp-tramp-file-p (buffer-file-name))
                                            (flycheck-mode))))



            (use-package sphinx-doc)
            (add-hook 'python-mode-hook (lambda ()
                                          (sphinx-doc-mode t)))

            (add-hook 'python-mode-hook 'pp:custom-jedi-setup)
            (add-hook 'inferior-python-mode-hook 'pp:custom-jedi-setup)

            (add-hook 'python-mode-hook
                      '(lambda () (eldoc-mode 1)) t)

            ))



(use-package virtualenvwrapper
  :defer 1
  :commands venv-workon
  :config
  (progn
    (venv-initialize-interactive-shells)
    (setq venv-location (expand-file-name "~/.venvs/"))
    ))

; Jedi
; TODO Jedi goto-definition is not working.
(use-package jedi
  :defer t
  :bind (("C-c d" . jedi:show-doc)
         ("M-SPC" . jedi:complete)
         ("M-." . jedi:goto-definition))
  :init
  (defun pp:custom-jedi-setup ()
    (jedi:setup)
    (jedi:ac-setup))

  :config (progn
            (setq jedi:install-server--command
                  `("pip2" "install" "--upgrade" ,(convert-standard-filename jedi:source-dir)))

            (setq jedi:server-command
                  `("python2" ,(concat jedi:source-dir "jediepcserver.py")))

            (setq jedi:setup-keys t
                  jedi:tooltip-method nil
                  jedi:get-in-function-call-delay 300
                  jedi:complete-on-dot t)))





; Yasnippet

(use-package yasnippet
;  :init (progn
                                        ;         (yas-global-mode 1))
  :defer 1
  :config (progn
            (setq yas-snippet-dirs '("~/.emacs.d/custom-snippets"
                                     "~/.emacs.d/packages/yasnippet-20150811.1222/snippets"))


            ; This could probably be more sophisticated
            (defun preview-fragment ()
              (if (looking-back "\) ")
                  (org-preview-latex-fragment)))

            (add-hook 'yas-after-exit-snippet-hook 'preview-fragment)
            (setq yas-triggers-in-field t)))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;    Org Mode
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (package-installed-p 'org-plus-contrib)
  (package-refresh-contents)
  (package-install 'org-plus-contrib))

(use-package org
  :ensure nil
  :defer 1
  :commands org-mode
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c C-x C-o" . org-clock-out)
         ("C-c a" . org-agenda)
         ("C-c b" . org-iswitchb))
  :init
  (progn
    (setq org-replace-disputed-keys t))

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
        (let ((file "~/org/journal.org")
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
          org-log-into-drawer t
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

;;          org-babel-use-quick-and-dirty-noweb-expansion t
          org-latex-pdf-process  '("latexmk -c"
                                   "pdflatex -interaction nonstopmode -output-directory %o %f"
                                   "biber --trace %b.bcf"
                                   "pdflatex -interaction nonstopmode -output-directory %o %f")

          org-src-preserve-indentation t
          org-src-fontify-natively t
          org-confirm-babel-evaluate nil)

    (setq org-enforce-todo-dependencies t
          org-agenda-dim-blocked-tasks t
          ;; Make sure sub-tasks inherit CREATED property
          org-use-property-inheritance "CREATED")


    (setq org-todo-keywords
          '((sequence "TODO(t)" "HOLD(h@/!)" "BACKLOG(b)" "|" "DONE(d!)" "INVALID(i@/!)")))


    (setq org-use-tag-inheritance '("nex" "gobig" "doeweb" "gumbo" "bd2k" "emacs" "chores"))

    (setq org-agenda-custom-commands
          '(("n" "Agenda and TODO's"
             ((agenda "")
              ;; in category kitware and not on hold
              ;; or tagged with kitware and not on hold
              (tags-todo (concat "CATEGORY=\"unfiled\"" "&" "-TODO=\"HOLD\""))
              (tags-todo (concat
                          "CATEGORY=\"kitware\"" "&" "-TODO=\"HOLD\"" "|"
                          "+kitware" "&" "-TODO=\"HOLD\""))
              (tags-todo (concat "TODO=\"HOLD\""))
              ))
            ))



    (defun org/cmp-created (a b)
      (let ((a_ts (org-float-time
                   (apply 'encode-time
                          (org-parse-time-string
                           (or (org-entry-get (get-text-property 1 'org-marker a) "CREATED")
                               "[1970-01-01 Thu]")))))
            (b_ts (org-float-time
                   (apply 'encode-time
                          (org-parse-time-string
                           (or (org-entry-get (get-text-property 1 'org-marker b) "CREATED")
                               "[1970-01-01 Thu]"))))))
        (cond ((< a_ts b_ts) -1)
              ((< b_ts a_ts) +1))))

    (setq org-agenda-cmp-user-defined 'org/cmp-created)

    (setq org-agenda-sorting-strategy
          '((agenda habit-down time-up priority-down category-keep)
            (todo priority-down category-keep)
            (tags user-defined-up)
            (search category-keep)))

    (setq org-capture-templates
          '(("t" "TODO" entry
             (file+headline "~/org/unfiled.org" "Tasks")
             "* TODO %? \n:PROPERTIES:\n:CREATED: %u\n:END:\n%i\n  %a")
            ("s" "Schedule" entry (file+headline "~/org/unfiled.org" "Meetings")
             "* %? \n:PROPERTIES:\n:CREATED: %u\n:END:\n  %i\n  %a")
            ("c" "Clock in" entry
             (file+datetree "~/org/journal.org")
             "* %U - %? %^g\n %i\n %a"
             :clock-in t :clock-keep t)
            ))

    (defun org/capture-add-id-hook ()
      (goto-char (point-min)) (org-id-get-create))

    (add-hook 'org-capture-mode-hook 'org/capture-add-id-hook)


    ; Additional packages and configurations
    (when window-system
      (require 'org-compat))

    (use-package ox-reveal
      :ensure nil)

    (use-package org-id
      :ensure nil
      :config (progn
                (setq org-id-link-to-org-use-id 'create-if-interactive-and-no-custom-id
                      org-link-to-org-use-id 'create-if-interactive-and-no-custom-id)))


    (use-package org-datetree
      :ensure nil)

    (use-package org-crypt
      :ensure nil
      :config (progn
                (org-crypt-use-before-save-magic)
                (setq org-tags-exclude-from-inheritance (quote ("crypt")))
                ;; GPG key to use for encryption
                ;; Either the Key ID or set to nil to use symmetric encryption.
                (setq org-crypt-key "E4CAD065")))

                                        ; Org Bullets
    (use-package org-bullets
      :ensure nil
      :init (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))



    (use-package org-protocol
      :ensure nil
      :config
      (progn
        (add-hook 'org-capture-after-finalize-hook 'org/capture-after-finalize)

        (add-to-list 'org-protocol-protocol-alist
                     '("Generic Org Capture"
                       :protocol "org-capture"
                       :function protocol-org-capture))))



    ; Misc Commands etc
    (org-add-link-type "claws" 'org-claws-link-command)

;;     (org-babel-do-load-languages
;;      'org-babel-load-languages
;;      '((emacs-lisp . t)
;;        (python . t)
;;        (R . t)
;;        ;(html . t)
;;        (latex . t)
;;        (sh . t )
;;        (sql . t)
;;        (org . t)
;;        (ditaa . t)
;;        (dot . t)))




    ;(define-key global-map "\C-cr" 'org-capture)

    ; Hooks

    (add-hook 'org-mode-hook (lambda () (org-yas-conflict)))
    ;(add-hook 'org-shiftup-final-hook 'windmove-up)
    ;(add-hook 'org-shiftleft-final-hook 'windmove-left)
    ;(add-hook 'org-shiftdown-final-hook 'windmove-down)
    ;(add-hook 'org-shiftright-final-hook 'windmove-right)

    (load (concat user-emacs-directory "lib/org-meta.el"))

    )) ; end org use-package




; Add 'zip' files to dired 'Z'
(eval-after-load "dired-aux"
   '(add-to-list 'dired-compress-file-suffixes
                 '("\\.zip\\'" ".zip" "unzip")))

(use-package markdown-mode
  :mode (("\\.md\\'" . markdown-mode)
         ("\\.markdown\\'" . markdown-mode)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;;;  Custom Functions
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun on-ec ()
  "run after launching via 'ec' script"
    (set-frame-parameter (selected-frame) 'alpha 0.9))


; Custom Functions
;(defun reload-browser ()
;  (interactive)
;  (shell-command (concat
;                 "CURRENT=$(xdotool getwindowfocus);"
;                 "xdotool search --onlyvisible --name chromium windowfocus key 'ctrl+r';"
;                 "xdotool windowactivate $CURRENT")))



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


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;  Breakpoint code

(require 'gud)

(defun pdb (command-line)
  "Run pdb on program FILE in buffer `*gud-FILE*'.
The directory containing FILE becomes the initial working directory
and source-file directory for your debugger."
  (interactive
   (list (gud-query-cmdline 'pdb)))

  (gud-common-init command-line nil 'gud-pdb-marker-filter)
  (set (make-local-variable 'gud-minor-mode) 'pdb)

  (gud-def gud-break       "break %d%f:%l"  "\C-b" "Set breakpoint at current line.")
  (gud-def gud-remove      "clear %d%f:%l"  "\C-d" "Remove breakpoint at current line")
  (gud-def gud-step        "step"         "\C-s" "Step one source line with display.")
  (gud-def gud-next        "next"         "\C-n" "Step one line (skip functions).")
  (gud-def gud-cont        "continue"     "\C-r" "Continue with display.")
  (gud-def gud-finish      "return"       "\C-f" "Finish executing current function.")
  (gud-def gud-up          "up"           "<" "Up one stack frame.")
  (gud-def gud-down        "down"         ">" "Down one stack frame.")
  (gud-def gud-print       "p %e"         "\C-p" "Evaluate Python expression at point.")
  ;; Is this right?
  (gud-def gud-statement "! %e"      "\C-e" "Execute Python statement at point.")

  ;; (setq comint-prompt-regexp "^(.*pdb[+]?) *")
  (setq comint-prompt-regexp "^(Pdb) *")
  (setq paragraph-start comint-prompt-regexp)
  (run-hooks 'pdb-mode-hook))

;; (toggle-highlight 181 (find-file-noselect "/home/kotfic/kitware/projects/src/romanesco/romanesco/__init__.py"))

(defun find-overlays-specifying (prop pos)
  (let ((overlays (overlays-at pos))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (overlay-get overlay prop)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))

(defun find-overlays (prop val pos)
  (let ((overlays (overlays-at pos))
        found)
    (while overlays
      (let ((overlay (car overlays)))
        (if (eq (overlay-get overlay prop) val)
            (setq found (cons overlay found))))
      (setq overlays (cdr overlays)))
    found))


(defun toggle-hl (line &optional buffer)
  (save-window-excursion
    (goto-line line buffer)
    (let ((beg (line-beginning-position))
          (end (+ 1 (line-end-position))))

      (if (find-overlays-specifying
           'line-highlight-overlay-marker (+ 1 beg))
          (remove-overlays beg end)

        (let ((overlay-highlight (make-overlay beg end)))
          (overlay-put overlay-highlight 'name 'breakpoint)
          (overlay-put overlay-highlight 'face '(:background "coral4"))
          (overlay-put overlay-highlight 'line-highlight-overlay-marker t))))))

(defun toggle-highlight ()
  (interactive)
  (save-restriction
    (widen)
    (save-excursion
      (beginning-of-line)
      (toggle-hl (1+ (count-lines 1 (point)))))))



; just for now so we can clean up
(global-set-key [f8] 'toggle-highlight)


(defvar pdb-breakpoint-locations '()
  "Keep a list of breakpoint locations")


(defun pdb/get-file-and-line ()
  "Return a tuple of type (file_path . line_number) for either the current
file and line number or the file path and line number of the current point
in the gud pdb buffer."
  (let* ((insource (not (eq (current-buffer) gud-comint-buffer)))
         (frame (or gud-last-frame gud-last-last-frame))
         (file (if insource (buffer-file-name) (car frame)))
         (line (if insource
                   (save-restriction
                     (widen)
                     (+ (count-lines (point-min) (point))
                        (if (bolp) 1 0)))
                 (cdr frame))))
    (if (stringp file)
        `(,file . ,line)
      nil)))

(defun pdb/add-breakpoint-overlay (file line)
  "Add a breakpoint overlay on the specified file and line"
  (if (file-exists-p file)
      (let ((buffer (find-file-noselect file)))
        (save-window-excursion
          (goto-line line buffer)
          (let ((beg (line-beginning-position))
                (end (+ 1 (line-end-position))))

            (if (not (find-overlays 'name 'breakpoint beg))
                (let ((overlay-highlight (make-overlay beg end)))
                  (overlay-put overlay-highlight 'name 'breakpoint)
                  (overlay-put overlay-highlight 'face '(:background "coral4"))
                  (overlay-put overlay-highlight 'line-highlight-overlay-marker t))))))
  (message (concat "Warning: file " file " does not exist; not adding overlay."))))

(defun pdb/remove-breakpoint-overlay (file line)
  "Remove breakpoing overlay on the specified file and line"
  (if (file-exists-p file)
      (let ((buffer (find-file-noselect file)))
        (save-window-excursion
          (goto-line line buffer)
          (let ((beg (line-beginning-position))
                (end (+ 1 (line-end-position))))
            (if (find-overlays 'name 'breakpoint beg)
                (remove-overlays beg end 'name 'breakpoint)))))))


(defun pdb/add-breakpoint ()
  "Add breakpoint overlay and add to pdb-breakpoint-locations"
  (let ((file_line (pdb/get-file-and-line)))
    (when file_line
      (add-to-list 'pdb-breakpoint-locations file_line)
      (pdb/add-breakpoint-overlay (car file_line) (cdr file_line)))))


(defun pdb/remove-breakpoint ()
  "remove breakpoint overlay and remove from pdb-breakpoint-locations"
  (let ((file_line (pdb/get-file-and-line)))
    (when file_line
      (setq pdb-breakpoint-locations
            (filter (lambda (bp)
                      (not (and
                            (equal (car bp) (car file_line))
                            (eq (cdr bp) (cdr file_line)))))
                    pdb-breakpoint-locations))
      (pdb/remove-breakpoint-overlay (car file_line) (cdr file_line)))))

(defun pdb/remove-all-breakpoint-overlays ()
  "Remove all breakpoint overlays in pdb-breakpoint-locations"
  (interactive)
  (mapc (lambda (bp)
            (let ((buffer (find-file-noselect (car bp))))
              (with-current-buffer buffer
                (remove-overlays nil nil 'name 'breakpoint))))
        pdb-breakpoint-locations))

(defun pdb/add-all-breakpoint-overlays ()
  "Add all breakpoint overlays in pdb-breakpoint-locatoins"
  (interactive)
  (mapc (lambda (bp)
          (pdb/add-breakpoint-overlay (car bp) (cdr bp)))
        pdb-breakpoint-locations))


(add-hook 'pdb-mode-hook
          (lambda ()
            ;; now commanet out - later add back in overlays
            ;; but also call pdb-break on files in pdb-breakpoint-locations
            ;; (pdb/add-all-breakpoint-overlays)

            (defadvice gud-break (after gud-breakpoint-hl activate)
              (pdb/add-breakpoint))

            (defadvice gud-remove (before gud-breakpoint-hl activate)
              (pdb/remove-breakpoint))

            (defadvice gud-sentinel (after gud-breakpoint-sentinal activate)
              ;; if we don't have an arrow,  we shouldn't have breakpoint overlays!
              (when (eq gud-overlay-arrow-position nil)
                (pdb/remove-all-breakpoint-overlays)

                ;; for now remove all locations - this is
                ;; gud/pdb's behavior - later remove this
                (setq pdb-breakpoint-locations '())
                ))

         ))


(global-set-key [f5]  (defhydra hydra-gud-pdb (:hint nil :color red)
"
                                                                       ╭─────────┐
                                                                       │ GUD PDB │
  ╭────────────────────────────────────────────────────────────────────┴─────────╯
    [_n_] Next Line            [_b_] Set Breakpoint          [_q_] Quit
    [_i_] Step Into            [_B_] Remove Breakpoint       [_p_] Print
    [_r_] Finish Function      [_<_] Move up Frame
    [_c_] Continue             [_>_] Move down Frame
  --------------------------------------------------------------------------------
"
        ("n" gud-next)
        ("i" gud-step)
        ("r" gud-finish)
        ("<" gud-up)
        (">" gud-down)
        ("c" gud-cont)
        ("b" gud-break)
        ("B" gud-remove)
        ("q" nil :exit t)
        ("p" (lambda ()
               (interactive)
               (unless (eq (current-buffer) gud-comint-buffer)
                 (switch-to-buffer-other-window gud-comint-buffer))
               (insert "pp ")) :exit t)
        ))



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
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
               ("weechat" (mode . weechat))
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


;; Require projects

(use-package defproject)
(add-to-list 'load-path (concat emacs-config-dir "/lib"))
(require 'projects)
(require 'wrapup)
(require 'local_configs)


(setq custom-file "~/.emacs.d/custom.el")
(load custom-file 'noerror)

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(ansi-color-names-vector
   ["#242424" "#e5786d" "#95e454" "#cae682" "#8ac6f2" "#333366" "#ccaa8f" "#f6f3e8"])
 '(custom-safe-themes
   (quote
    ("a186f1520b7317917dadae0b2b07659c2c9485f1e615b1e93e8f82eb9d18f936" "c0d2fcf6d2907aa90ab5388865f9e0befbfba81f0afb555fdd154fc871ef0fc6" "90d329edc17c6f4e43dbc67709067ccd6c0a3caa355f305de2041755986548f2" "26614652a4b3515b4bbbb9828d71e206cc249b67c9142c06239ed3418eff95e2" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" default)))
 '(fci-rule-color "#37474f")
 '(fringe-mode 10 nil (fringe))
 '(ido-default-buffer-method (quote selected-window))
 '(js2-mode-show-parse-errors nil)
 '(js2-strict-missing-semi-warning nil)
 '(linum-format " %6d ")
 '(main-line-color1 "#222912")
 '(main-line-color2 "#09150F")
 '(notmuch-saved-searches
   (quote
    ((:name "inbox" :query "tag:inbox" :key "i")
     (:name "unread" :query "tag:unread" :key "u")
     (:name "flagged" :query "tag:flagged" :key "f")
     (:name "sent" :query "tag:sent" :key "t")
     (:name "drafts" :query "tag:draft" :key "d")
     (:name "all mail" :query "*" :key "a")
     (:name "kitware" :query "path:kitware/**"))))
 '(package-selected-packages
   (quote
    (pycoverage pkgbuild-mode yaml-mode weechat virtualenvwrapper use-package twittering-mode stylus-mode sphinx-doc scala-mode2 sauron rainbow-mode prodigy powerline php-mode ox-reveal org-trello org-plus-contrib org-pdfview org-bullets noflet markdown-mode magit link-hint js2-refactor jedi jade-mode hydra helm-swoop helm-projectile helm-descbinds helm-dash helm-ag guide-key git-timemachine flycheck ensime elfeed-org defproject company-tern color-theme-sanityinc-tomorrow color-theme-monokai cmake-mode badger-theme)))
 '(powerline-color1 "#222912")
 '(powerline-color2 "#09150F")
 '(safe-local-variable-values
   (quote
    ((projectile-project-test-cmd . "cd /home/kotfic/kitware/projects/src/girder_worker/_build/ && ctest -E \"scala|arbor|swift\"")
     (eval venv-workon "girder")
     (projectile-project-test-cmd . "cd /home/kotfic/kitware/projects/src/build/girder/ && ctest -j8")
     (flycheck-flake8rc . "/home/kotfic/kitware/projects/HPCCloud/src/cumulustests/flake8.cfg")
     (flycheck-python-flake8-executable . "/home/kotfic/.venvs/cumulus-deploy/bin/flake8")
     (eval venv-workon "cumulus-deploy")
     (projectile-project-test-cmd . "cd /home/kotfic/kitware/projects/NEX/src/build/girder/ && ctest -j8 -R minerva")
     (org-clock-into-drawer . t)
     (flycheck-flake8rc . "/home/kotfic/kitware/projects/src/romanesco/tests/flake8.cfg")
     (projectile-project-test-cmd . "cd /home/kotfic/kitware/projects/src/build/romanesco/  && ctest -j8")
     (eval setenv "PYTHONPATH"
           (concat
            (expand-file-name "/home/kotfic/kitware/projects/src/VTK/build/Wrapping/Python")
            ":"
            (expand-file-name "/home/kotfic/kitware/projects/src/VTK/build/lib")))
     (flycheck-flake8rc . "/home/kotfic/kitware/projects/NEX/src/girder/girder/tests/flake8.cfg")
     (flycheck-python-flake8-executable . "/home/kotfic/.venvs/NEX/bin/flake8")
     (eval venv-workon "NEX")
     (flycheck-jscsrc . "/home/kotfic/kitware/projects/NEX/src/build/girder/tests/minerva_jsstyle.cfg")
     (flycheck-jshintrc . "/home/kotfic/kitware/projects/NEX/src/build/girder/tests/minerva_jshint.cfg")
     (projectile-project-test-cmd . "cd /home/kotfic/kitware/projects/NEX/src/build/girder/ && ctest -j8"))))
 '(sml/mode-width
   (if
       (eq powerline-default-separator
           (quote arrow))
       (quote right)
     (quote full)) t)
 '(sml/pos-id-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (car powerline-default-separator-dir)))
                   (quote powerline-active1)
                   (quote powerline-active2))))
     (:propertize " " face powerline-active2))))
 '(sml/pos-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active1)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active1)
                   nil)))
     (:propertize " " face sml/global))))
 '(sml/pre-id-separator
   (quote
    (""
     (:propertize " " face sml/global)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (car powerline-default-separator-dir)))
                   nil
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-minor-modes-separator
   (quote
    (""
     (:propertize " " face powerline-active2)
     (:eval
      (propertize " "
                  (quote display)
                  (funcall
                   (intern
                    (format "powerline-%s-%s" powerline-default-separator
                            (cdr powerline-default-separator-dir)))
                   (quote powerline-active2)
                   (quote powerline-active1))))
     (:propertize " " face powerline-active1))))
 '(sml/pre-modes-separator (propertize " " (quote face) (quote sml/modes)))
 '(vc-annotate-background nil)
 '(vc-annotate-color-map
   (quote
    ((20 . "#f36c60")
     (40 . "#ff9800")
     (60 . "#fff59d")
     (80 . "#8bc34a")
     (100 . "#81d4fa")
     (120 . "#4dd0e1")
     (140 . "#b39ddb")
     (160 . "#f36c60")
     (180 . "#ff9800")
     (200 . "#fff59d")
     (220 . "#8bc34a")
     (240 . "#81d4fa")
     (260 . "#4dd0e1")
     (280 . "#b39ddb")
     (300 . "#f36c60")
     (320 . "#ff9800")
     (340 . "#fff59d")
     (360 . "#8bc34a"))))
 '(vc-annotate-very-old-color nil))

;(when (member "Source Code Pro" (font-family-list))
;  (custom-set-faces
;   '(default ((t (:family "Source Code Pro"))))
   ;; custom-set-faces was added by Custom.
   ;; If you edit it by hand, you could mess it up, so be careful.
   ;; Your init file should contain only one such instance.
   ;; If there is more than one, they won't work right.

;   ))
(put 'scroll-left 'disabled nil)
(put 'upcase-region 'disabled nil)
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(notmuch-search-unread-face ((t (:foreground "light sky blue")))))

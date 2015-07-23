
(use-package-ensure js2-mode
  ;; flycheck does checking for jshint and jscs
  :config (progn
            (custom-set-variables
             '(js2-mode-show-parse-errors nil)
             '(js2-strict-missing-semi-warning nil))

            (add-hook 'js2-mode-hook 'js2-refactor-mode)
            (add-hook 'js2-mode-hook 'rainbow-mode)
            (add-hook 'js2-mode-hook 'company-mode)
            (add-hook 'js2-mode-hook 'tern-mode)
            (add-hook 'js2-mode-hook 'flycheck-mode))
  :mode ("\\.js\\'" . js2-mode))


;; js2-refactor
(use-package-ensure js2-refactor)

;; rainbow-mode
(use-package-ensure rainbow-mode
		    :pin gnu)

;; company
(use-package-ensure company)

;; company-tern
(use-package-ensure company-tern
  :config (progn
            (add-to-list 'company-backends 'company-tern)))

;; flycheck
(use-package-ensure flycheck)

;; tern
(use-package-ensure tern)


;; jade-mode
(use-package-ensure jade-mode)


;; stylus-mode
(use-package-ensure stylus-mode
  :config (progn
            (add-hook 'stylus-mode-hook 'rainbow-mode)))

(provide 'tern_dan)

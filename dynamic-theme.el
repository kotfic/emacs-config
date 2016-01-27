;;; dynamic-theme.el --- A templateable theme for Emacs 24.  -*- lexical-binding: t; -*-

;; Copyright (C) 2016 Chris Kotfila

;; Author: Chris Kotfila
;; Package-Version: 0.0.0
;; Version: 0.0.0

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:
;; Fonts setup based on https://github.com/purcell/color-theme-sanityinc-tomorrow
;; General code structure borrowed from badger-theme.el https://github.com/ccann/badger-theme

;;; Credits:

;; The structure of this theme was based on zenburn-theme.el by Bozhidar Batsov

;;; Code:

;; default

(deftheme dynamic "Dynamic theme managed by movein")

(defvar dynamic-theme-colors-alist
  '(("background" . "#1d1f21")
    ("current-line" . "#282a2e")
    ("selection" . "#373b41")
    ("foreground" . "#c5c8c6")
    ("comment" . "#999999")
    ("red" . "#cc6666")
    ("orange" . "#e78c45")
    ("yellow" . "#f0c674")
    ("green" . "#b5bd68")
    ("aqua" . "#8abeb7")
    ("blue" . "#81a2be")
    ("purple" . "#c397d8")))

(defmacro dynamic/with-color-variables (&rest body)
  "`let' bind all colors defined in `badger-colors-alist' around BODY.
Also bind `class' to ((class color) (min-colors 89))."
  (declare (indent 0))
  `(let ((class '((class color) (min-colors 89)))
         ,@(mapcar (lambda (cons)
                     (list (intern (car cons)) (cdr cons)))
                   dynamic-theme-colors-alist))
     ,@body))

(dynamic/with-color-variables
  (custom-theme-set-faces
   'dynamic
   `(default ((,class (:foreground ,foreground :background ,background))) t)
   `(bold ((,class (:weight bold))) t)
   `(bold-italic ((,class (:slant italic :weight bold))) t)
   `(underline ((,class (:underline t))) t)
   `(italic ((,class (:slant italic))) t)
   `(font-lock-builtin-face ((,class (:foreground ,purple))) t)
   `(font-lock-comment-delimiter-face ((,class (:foreground ,comment :slant italic))) t)
   `(font-lock-comment-face ((,class (:foreground ,comment :slant italic))) t)
   `(font-lock-constant-face ((,class (:foreground ,blue))) t)
   `(font-lock-doc-face ((,class (:foreground ,purple))) t)
   `(font-lock-doc-string-face ((,class (:foreground ,yellow))) t)
   `(font-lock-function-name-face ((,class (:foreground ,orange))) t)
   `(font-lock-keyword-face ((,class (:foreground ,green))) t)
   `(font-lock-negation-char-face ((,class (:foreground ,blue))) t)
   `(font-lock-preprocessor-face ((,class (:foreground ,purple))) t)
   `(font-lock-regexp-grouping-backslash ((,class (:foreground ,yellow))) t)
   `(font-lock-regexp-grouping-construct ((,class (:foreground ,purple))) t)
   `(font-lock-string-face ((,class (:foreground ,aqua))) t)
   `(font-lock-type-face ((,class (:foreground ,blue))) t)
   `(font-lock-variable-name-face ((,class (:foreground ,yellow))) t)
   `(font-lock-warning-face ((,class (:weight bold :foreground ,red))) t)
   `(shadow ((,class (:foreground ,comment))) t)
   `(success ((,class (:foreground ,green))) t)
   `(error ((,class (:foreground ,red))) t)
   `(warning ((,class (:foreground ,orange))) t)
   `(outline-4 ((,class (:slant normal :foreground ,comment))) t)

   ;; Flymake
   `(flymake-warnline ((,class (:underline ,orange :background ,background))) t)
   `(flymake-errline ((,class (:underline ,red :background ,background))) t)

   ;; Clojure errors
   `(clojure-test-failure-face ((,class (:background nil :inherit flymake-warnline))) t)
   `(clojure-test-error-face ((,class (:background nil :inherit flymake-errline))) t)
   `(clojure-test-success-face ((,class (:background nil :foreground nil :underline ,green))) t)

   ;; EDTS errors
   `(edts-face-warning-line ((t (:background nil :inherit flymake-warnline))) t)
   `(edts-face-warning-mode-line ((,class (:background nil :foreground ,orange :weight bold))) t)
   `(edts-face-error-line ((t (:background nil :inherit flymake-errline))) t)
   `(edts-face-error-mode-line ((,class (:background nil :foreground ,red :weight bold))) t)

   ;; For Brian Carper's extended clojure syntax table
   `(clojure-keyword ((,class (:foreground ,yellow))) t)
   `(clojure-parens ((,class (:foreground ,foreground))) t)
   `(clojure-braces ((,class (:foreground ,green))) t)
   `(clojure-brackets ((,class (:foreground ,yellow))) t)
   `(clojure-double-quote ((,class (:foreground ,aqua :background nil))) t)
   `(clojure-special ((,class (:foreground ,blue))) t)
   `(clojure-java-call ((,class (:foreground ,purple))) t)

     ;; Rainbow-delimiters
   `(rainbow-delimiters-depth-1-face ((,class (:foreground ,foreground))) t)
   `(rainbow-delimiters-depth-2-face ((,class (:foreground ,aqua))) t)
   `(rainbow-delimiters-depth-3-face ((,class (:foreground ,yellow))) t)
   `(rainbow-delimiters-depth-4-face ((,class (:foreground ,green))) t)
   `(rainbow-delimiters-depth-5-face ((,class (:foreground ,blue))) t)
   `(rainbow-delimiters-depth-6-face ((,class (:foreground ,foreground))) t)
   `(rainbow-delimiters-depth-7-face ((,class (:foreground ,aqua))) t)
   `(rainbow-delimiters-depth-8-face ((,class (:foreground ,yellow))) t)
   `(rainbow-delimiters-depth-9-face ((,class (:foreground ,green))) t)
   `(rainbow-delimiters-unmatched-face ((,class (:foreground ,red))) t)

     ;; MMM-mode
   `(mmm-code-submode-face ((,class (:background ,current-line))) t)
   `(mmm-comment-submode-face ((,class (:inherit font-lock-comment-face))) t)
   `(mmm-output-submode-face ((,class (:background ,current-line))) t)

     ;; Search
   `(match ((,class (:foreground ,blue :background ,background :inverse-video t))) t)
   `(isearch ((,class (:foreground ,yellow :background ,background :inverse-video t))) t)
   `(isearch-lazy-highlight-face ((,class (:foreground ,aqua :background ,background :inverse-video t))) t)
   `(isearch-fail ((,class (:background ,background :inherit font-lock-warning-face :inverse-video t))) t)

     ;; IDO
   `(ido-subdir ((,class (:foreground ,purple))) t)
   `(ido-first-match ((,class (:foreground ,orange))) t)
   `(ido-only-match ((,class (:foreground ,green))) t)
   `(ido-indicator ((,class (:foreground ,red :background ,background))) t)
   `(ido-virtual ((,class (:foreground ,comment))) t)

     ;; which-function
   `(which-func ((,class (:foreground ,blue :background nil :weight bold))) t)

     ;; Emacs interface
   `(cursor ((,class (:background ,red))) t)
   `(fringe ((,class (:background ,current-line))) t)
   `(linum ((,class (:background ,current-line))) t)
   `(border ((,class (:background ,current-line))) t)
   `(border-glyph ((,class (nil))) t)
   `(highlight ((,class (:inverse-video nil :background ,current-line))) t)
   `(gui-element ((,class (:background ,current-line :foreground ,foreground))) t)
   `(mode-line ((,class (:foreground nil :background ,current-line
                                     :box (:line-width 1 :color ,foreground)))) t)
   `(mode-line-buffer-id ((,class (:foreground ,purple :background nil))) t)
   `(mode-line-inactive ((,class (:inherit mode-line
                                       :foreground ,comment
                                       :background ,current-line :weight normal
                                       :box (:line-width 1 :color ,foreground)))) t)
   `(mode-line-emphasis ((,class (:foreground ,foreground :slant italic))) t)
   `(mode-line-highlight ((,class (:foreground ,purple :box nil :weight bold))) t)
   `(minibuffer-prompt ((,class (:foreground ,blue))) t)
   `(region ((,class (:background ,selection))) t)
   `(secondary-selection ((,class (:background ,current-line))) t)

   `(header-line ((,class (:inherit mode-line :foreground ,purple :background nil))) t)

   `(trailing-whitespace ((,class (:foreground ,red :inverse-video t :underline nil))) t)
   `(whitespace-trailing ((,class (:foreground ,red :inverse-video t :underline nil))) t)
   `(whitespace-space-after-tab ((,class (:foreground ,red :inverse-video t :underline nil))) t)
   `(whitespace-space-before-tab ((,class (:foreground ,red :inverse-video t :underline nil))) t)
   `(whitespace-empty ((,class (:foreground ,red :inverse-video t :underline nil))) t)
   `(whitespace-line ((,class (:background nil :foreground ,red))) t)
   `(whitespace-indentation ((,class (:background nil :foreground ,aqua))) t)
   `(whitespace-space ((,class (:background nil :foreground ,selection))) t)
   `(whitespace-newline ((,class (:background nil :foreground ,selection))) t)
   `(whitespace-tab ((,class (:background nil :foreground ,selection))) t)
   `(whitespace-hspace ((,class (:background nil :foreground ,selection))) t)

     ;; Parenthesis matching (built-in)
   `(show-paren-match ((,class (:background nil :foreground nil :inverse-video t))) t)
   `(show-paren-mismatch ((,class (:background ,purple :foreground ,background))) t)

     ;; Parenthesis matching (mic-paren)
   `(paren-face-match ((,class (:foreground nil :background nil :inherit show-paren-match))) t)
   `(paren-face-mismatch ((,class (:foreground nil :background nil :inherit show-paren-mismatch))) t)
   `(paren-face-no-match ((,class (:foreground nil :background nil :inherit show-paren-mismatch))) t)

     ;; Parenthesis dimming (parenface)
   `(paren-face ((,class (:foreground ,comment :background nil))) t)

   `(sh-heredoc ((,class (:foreground nil :inherit font-lock-string-face :weight normal))) t)
   `(sh-quoted-exec ((,class (:foreground nil :inherit font-lock-preprocessor-face))) t)
   `(slime-highlight-edits-face ((,class (:weight bold))) t)
   `(slime-repl-input-face ((,class (:weight normal :underline nil))) t)
   `(slime-repl-prompt-face ((,class (:underline nil :weight bold :foreground ,purple))) t)
   `(slime-repl-result-face ((,class (:foreground ,green))) t)
   `(slime-repl-output-face ((,class (:foreground ,blue :background ,background))) t)

   `(csv-separator-face ((,class (:foreground ,orange))) t)

   `(diff-added ((,class (:foreground ,green))) t)
   `(diff-changed ((,class (:foreground ,purple))) t)
   `(diff-removed ((,class (:foreground ,orange))) t)
   `(diff-header ((,class (:foreground ,aqua :background nil))) t)
   `(diff-file-header ((,class (:foreground ,blue :background nil))) t)
   `(diff-hunk-header ((,class (:foreground ,purple))) t)
   `(diff-refine-added ((,class (:inherit diff-added :inverse-video t))) t)
   `(diff-refine-removed ((,class (:inherit diff-removed :inverse-video t))) t)

   `(ediff-even-diff-A ((,class (:foreground nil :background nil :inverse-video t))) t)
   `(ediff-even-diff-B ((,class (:foreground nil :background nil :inverse-video t))) t)
   `(ediff-odd-diff-A  ((,class (:foreground ,comment :background nil :inverse-video t))) t)
   `(ediff-odd-diff-B  ((,class (:foreground ,comment :background nil :inverse-video t))) t)

   `(eldoc-highlight-function-argument ((,class (:foreground ,green :weight bold))) t)

     ;; macrostep
   `(macrostep-expansion-highlight-face ((,class (:inherit highlight :foreground nil))) t)

     ;; undo-tree
   `(undo-tree-visualizer-default-face ((,class (:foreground ,foreground))) t)
   `(undo-tree-visualizer-current-face ((,class (:foreground ,green :weight bold))) t)
   `(undo-tree-visualizer-active-branch-face ((,class (:foreground ,red))) t)
   `(undo-tree-visualizer-register-face ((,class (:foreground ,yellow))) t)

     ;; dired+
   `(diredp-compressed-file-suffix ((,class (:foreground ,blue))) t)
   `(diredp-dir-heading ((,class (:foreground nil :background nil :inherit heading))) t)
   `(diredp-dir-priv ((,class (:foreground ,aqua :background nil))) t)
   `(diredp-exec-priv ((,class (:foreground ,blue :background nil))) t)
   `(diredp-executable-tag ((,class (:foreground ,red :background nil))) t)
   `(diredp-file-name ((,class (:foreground ,yellow))) t)
   `(diredp-file-suffix ((,class (:foreground ,green))) t)
   `(diredp-flag-mark-line ((,class (:background nil :inherit highlight))) t)
   `(diredp-ignored-file-name ((,class (:foreground ,comment))) t)
   `(diredp-link-priv ((,class (:background nil :foreground ,purple))) t)
   `(diredp-mode-line-flagged ((,class (:foreground ,red))) t)
   `(diredp-mode-line-marked ((,class (:foreground ,green))) t)
   `(diredp-no-priv ((,class (:background nil))) t)
   `(diredp-number ((,class (:foreground ,yellow))) t)
   `(diredp-other-priv ((,class (:background nil :foreground ,purple))) t)
   `(diredp-rare-priv ((,class (:foreground ,red :background nil))) t)
   `(diredp-read-priv ((,class (:foreground ,green :background nil))) t)
   `(diredp-symlink ((,class (:foreground ,purple))) t)
   `(diredp-write-priv ((,class (:foreground ,yellow :background nil))) t)

     ;; Magit (a patch is pending in magit to make these standard upstream)
   `(magit-branch ((,class (:foreground ,green))) t)
   `(magit-header ((,class (:inherit nil :weight bold))) t)
   `(magit-item-highlight ((,class (:inherit highlight :background nil))) t)
   `(magit-log-author ((,class (:foreground ,aqua))) t)
   `(magit-log-graph ((,class (:foreground ,comment))) t)
   `(magit-log-sha1 ((,class (:foreground ,yellow))) t)
   `(magit-log-head-label-bisect-bad ((,class (:foreground ,red))) t)
   `(magit-log-head-label-bisect-good ((,class (:foreground ,green))) t)
   `(magit-log-head-label-default ((,class (:foreground ,yellow :box nil :weight bold))) t)
   `(magit-log-head-label-local ((,class (:foreground ,purple :box nil :weight bold))) t)
   `(magit-log-head-label-remote ((,class (:foreground ,purple :box nil :weight bold))) t)
   `(magit-log-head-label-tags ((,class (:foreground ,aqua :box nil :weight bold))) t)
   `(magit-section-title ((,class (:foreground ,blue :weight bold))) t)

     ;; git-gutter
   `(git-gutter:modified ((,class (:foreground ,purple :weight bold))) t)
   `(git-gutter:added ((,class (:foreground ,green :weight bold))) t)
   `(git-gutter:deleted ((,class (:foreground ,red :weight bold))) t)
   `(git-gutter:unchanged ((,class (:background ,yellow))) t)

     ;; git-gutter-fringe
   `(git-gutter-fr:modified ((,class (:foreground ,purple :weight bold))) t)
   `(git-gutter-fr:added ((,class (:foreground ,green :weight bold))) t)
   `(git-gutter-fr:deleted ((,class (:foreground ,red :weight bold))) t)

   `(link ((,class (:foreground nil :underline t))) t)
   `(widget-button ((,class (:underline t))) t)
   `(widget-field ((,class (:background ,current-line :box (:line-width 1 :color ,foreground)))) t)

     ;; Compilation (most faces politely inherit from 'success, 'error, 'warning etc.)
   `(compilation-column-number ((,class (:foreground ,yellow))) t)
   `(compilation-line-number ((,class (:foreground ,yellow))) t)
   `(compilation-message-face ((,class (:foreground ,blue))) t)
   `(compilation-mode-line-exit ((,class (:foreground ,green))) t)
   `(compilation-mode-line-fail ((,class (:foreground ,red))) t)
   `(compilation-mode-line-run ((,class (:foreground ,blue))) t)

     ;; Grep
   `(grep-context-face ((,class (:foreground ,comment))) t)
   `(grep-error-face ((,class (:foreground ,red :weight bold :underline t))) t)
   `(grep-hit-face ((,class (:foreground ,blue))) t)
   `(grep-match-face ((,class (:foreground nil :background nil :inherit match))) t)

   `(regex-tool-matched-face ((,class (:foreground nil :background nil :inherit match))) t)

     ;; mark-multiple
   `(mm/master-face ((,class (:inherit region :foreground nil :background nil))) t)
   `(mm/mirror-face ((,class (:inherit region :foreground nil :background nil))) t)

   `(org-agenda-structure ((,class (:foreground ,purple))) t)
   `(org-agenda-date ((,class (:foreground ,blue :underline nil))) t)
   `(org-agenda-done ((,class (:foreground ,green))) t)
   `(org-agenda-dimmed-todo-face ((,class (:foreground ,comment))) t)
   `(org-block ((,class (:foreground ,orange))) t)
   `(org-code ((,class (:foreground ,yellow))) t)
   `(org-column ((,class (:background ,current-line))) t)
   `(org-column-title ((,class (:inherit org-column :weight bold :underline t))) t)
   `(org-date ((,class (:foreground ,blue :underline t))) t)
   `(org-document-info ((,class (:foreground ,aqua))) t)
   `(org-document-info-keyword ((,class (:foreground ,green))) t)
   `(org-document-title ((,class (:weight bold :foreground ,orange :height 1.44))) t)
   `(org-done ((,class (:foreground ,green))) t)
   `(org-ellipsis ((,class (:foreground ,comment))) t)
   `(org-footnote ((,class (:foreground ,aqua))) t)
   `(org-formula ((,class (:foreground ,red))) t)
   `(org-hide ((,class (:foreground ,background :background ,background))) t)
   `(org-link ((,class (:foreground ,blue :underline t))) t)
   `(org-scheduled ((,class (:foreground ,green))) t)
   `(org-scheduled-previously ((,class (:foreground ,orange))) t)
   `(org-scheduled-today ((,class (:foreground ,green))) t)
   `(org-special-keyword ((,class (:foreground ,orange))) t)
   `(org-table ((,class (:foreground ,purple))) t)
   `(org-todo ((,class (:foreground ,red))) t)
   `(org-upcoming-deadline ((,class (:foreground ,orange))) t)
   `(org-warning ((,class (:weight bold :foreground ,red))) t)

   `(markdown-url-face ((,class (:inherit link))) t)
   `(markdown-link-face ((,class (:foreground ,blue :underline t))) t)

   `(hl-sexp-face ((,class (:background ,current-line))) t)
   `(highlight-80+ ((,class (:background ,current-line))) t)

     ;; Python-specific overrides
   `(py-builtins-face ((,class (:foreground ,orange :weight normal))) t)

     ;; js2-mode
   `(js2-warning ((,class (:underline ,orange))) t)
   `(js2-error ((,class (:foreground nil :underline ,red))) t)
   `(js2-external-variable ((,class (:foreground ,purple))) t)
   `(js2-function-param ((,class (:foreground ,blue))) t)
   `(js2-instance-member ((,class (:foreground ,blue))) t)
   `(js2-private-function-call ((,class (:foreground ,red))) t)

     ;; js3-mode
   `(js3-warning-face ((,class (:underline ,orange))) t)
   `(js3-error-face ((,class (:foreground nil :underline ,red))) t)
   `(js3-external-variable-face ((,class (:foreground ,purple))) t)
   `(js3-function-param-face ((,class (:foreground ,blue))) t)
   `(js3-jsdoc-tag-face ((,class (:foreground ,orange))) t)
   `(js3-jsdoc-type-face ((,class (:foreground ,aqua))) t)
   `(js3-jsdoc-value-face ((,class (:foreground ,yellow))) t)
   `(js3-jsdoc-html-tag-name-face ((,class (:foreground ,blue))) t)
   `(js3-jsdoc-html-tag-delimiter-face ((,class (:foreground ,green))) t)
   `(js3-instance-member-face ((,class (:foreground ,blue))) t)
   `(js3-private-function-call-face ((,class (:foreground ,red))) t)

     ;; coffee-mode
   `(coffee-mode-class-name ((,class (:foreground ,orange :weight bold))) t)
   `(coffee-mode-function-param ((,class (:foreground ,purple))) t)

     ;; nxml
   `(nxml-name-face ((,class (:foreground unspecified :inherit font-lock-constant-face))) t)
   `(nxml-attribute-local-name-face ((,class (:foreground unspecified :inherit font-lock-variable-name-face))) t)
   `(nxml-ref-face ((,class (:foreground unspecified :inherit font-lock-preprocessor-face))) t)
   `(nxml-delimiter-face ((,class (:foreground unspecified :inherit font-lock-keyword-face))) t)
   `(nxml-delimited-data-face ((,class (:foreground unspecified :inherit font-lock-string-face))) t)
   `(rng-error-face ((,class (:underline ,red))) t)

   ;; RHTML
   `(erb-delim-face ((,class (:background ,current-line))) t)
   `(erb-exec-face ((,class (:background ,current-line :weight bold))) t)
   `(erb-exec-delim-face ((,class (:background ,current-line))) t)
   `(erb-out-face ((,class (:background ,current-line :weight bold))) t)
   `(erb-out-delim-face ((,class (:background ,current-line))) t)
   `(erb-comment-face ((,class (:background ,current-line :weight bold :slant italic))) t)
   `(erb-comment-delim-face ((,class (:background ,current-line))) t)

     ;; Message-mode
   `(message-header-other ((,class (:foreground nil :background nil :weight normal))) t)
   `(message-header-subject ((,class (:inherit message-header-other :weight bold :foreground ,yellow))) t)
   `(message-header-to ((,class (:inherit message-header-other :weight bold :foreground ,orange))) t)
   `(message-header-cc ((,class (:inherit message-header-to :foreground nil))) t)
   `(message-header-name ((,class (:foreground ,blue :background nil))) t)
   `(message-header-newsgroups ((,class (:foreground ,aqua :background nil :slant normal))) t)
   `(message-separator ((,class (:foreground ,purple))) t)

     ;; Jabber
   `(jabber-chat-prompt-local ((,class (:foreground ,yellow))) t)
   `(jabber-chat-prompt-foreign ((,class (:foreground ,orange))) t)
   `(jabber-chat-prompt-system ((,class (:foreground ,yellow :weight bold))) t)
   `(jabber-chat-text-local ((,class (:foreground ,yellow))) t)
   `(jabber-chat-text-foreign ((,class (:foreground ,orange))) t)
   `(jabber-chat-text-error ((,class (:foreground ,red))) t)

   `(jabber-roster-user-online ((,class (:foreground ,green))) t)
   `(jabber-roster-user-xa ((,class :foreground ,comment)) t)
   `(jabber-roster-user-dnd ((,class :foreground ,yellow)) t)
   `(jabber-roster-user-away ((,class (:foreground ,orange))) t)
   `(jabber-roster-user-chatty ((,class (:foreground ,purple))) t)
   `(jabber-roster-user-error ((,class (:foreground ,red))) t)
   `(jabber-roster-user-offline ((,class (:foreground ,comment))) t)

   `(jabber-rare-time-face ((,class (:foreground ,comment))) t)
   `(jabber-activity-face ((,class (:foreground ,purple))) t)
   `(jabber-activity-personal-face ((,class (:foreground ,aqua))) t)

     ;; Powerline
   `(powerline-active1 ((t (:foreground ,foreground :background ,selection))) t)
   `(powerline-active2 ((t (:foreground ,foreground :background ,current-line))) t)

     ;; Outline
   `(outline-1 ((,class (:inherit nil :foreground ,blue))) t)
   `(outline-2 ((,class (:inherit nil :foreground ,yellow))) t)
   `(outline-3 ((,class (:inherit nil :foreground ,purple))) t)
   `(outline-4 ((,class (:inherit nil :foreground ,aqua))) t)
   `(outline-5 ((,class (:inherit nil :foreground ,orange))) t)
   `(outline-6 ((,class (:inherit nil :foreground ,blue))) t)
   `(outline-7 ((,class (:inherit nil :foreground ,yellow))) t)
   `(outline-8 ((,class (:inherit nil :foreground ,purple))) t)
   `(outline-9 ((,class (:inherit nil :foreground ,aqua))) t)

     ;; Gnus
   `(gnus-cite-1 ((,class (:inherit outline-1 :foreground nil))) t)
   `(gnus-cite-2 ((,class (:inherit outline-2 :foreground nil))) t)
   `(gnus-cite-3 ((,class (:inherit outline-3 :foreground nil))) t)
   `(gnus-cite-4 ((,class (:inherit outline-4 :foreground nil))) t)
   `(gnus-cite-5 ((,class (:inherit outline-5 :foreground nil))) t)
   `(gnus-cite-6 ((,class (:inherit outline-6 :foreground nil))) t)
   `(gnus-cite-7 ((,class (:inherit outline-7 :foreground nil))) t)
   `(gnus-cite-8 ((,class (:inherit outline-8 :foreground nil))) t)
     ;; there are several more -cite- faces...
   `(gnus-header-content ((,class (:inherit message-header-other))) t)
   `(gnus-header-subject ((,class (:inherit message-header-subject))) t)
   `(gnus-header-from ((,class (:inherit message-header-other-face :weight bold :foreground ,orange))) t)
   `(gnus-header-name ((,class (:inherit message-header-name))) t)
   `(gnus-button ((,class (:inherit link :foreground nil))) t)
   `(gnus-signature ((,class (:inherit font-lock-comment-face))) t)

   `(gnus-summary-normal-unread ((,class (:foreground ,blue :weight normal))) t)
   `(gnus-summary-normal-read ((,class (:foreground ,foreground :weight normal))) t)
   `(gnus-summary-normal-ancient ((,class (:foreground ,aqua :weight normal))) t)
   `(gnus-summary-normal-ticked ((,class (:foreground ,orange :weight normal))) t)
   `(gnus-summary-low-unread ((,class (:foreground ,comment :weight normal))) t)
   `(gnus-summary-low-read ((,class (:foreground ,comment :weight normal))) t)
   `(gnus-summary-low-ancient ((,class (:foreground ,comment :weight normal))) t)
   `(gnus-summary-high-unread ((,class (:foreground ,yellow :weight normal))) t)
   `(gnus-summary-high-read ((,class (:foreground ,green :weight normal))) t)
   `(gnus-summary-high-ancient ((,class (:foreground ,green :weight normal))) t)
   `(gnus-summary-high-ticked ((,class (:foreground ,orange :weight normal))) t)
   `(gnus-summary-cancelled ((,class (:foreground ,red :background nil :weight normal))) t)

   `(gnus-group-mail-low ((,class (:foreground ,comment))) t)
   `(gnus-group-mail-low-empty ((,class (:foreground ,comment))) t)
   `(gnus-group-mail-1 ((,class (:foreground nil :weight normal :inherit outline-1))) t)
   `(gnus-group-mail-2 ((,class (:foreground nil :weight normal :inherit outline-2))) t)
   `(gnus-group-mail-3 ((,class (:foreground nil :weight normal :inherit outline-3))) t)
   `(gnus-group-mail-4 ((,class (:foreground nil :weight normal :inherit outline-4))) t)
   `(gnus-group-mail-5 ((,class (:foreground nil :weight normal :inherit outline-5))) t)
   `(gnus-group-mail-6 ((,class (:foreground nil :weight normal :inherit outline-6))) t)
   `(gnus-group-mail-1-empty ((,class (:inherit gnus-group-mail-1 :foreground ,comment))) t)
   `(gnus-group-mail-2-empty ((,class (:inherit gnus-group-mail-2 :foreground ,comment))) t)
   `(gnus-group-mail-3-empty ((,class (:inherit gnus-group-mail-3 :foreground ,comment))) t)
   `(gnus-group-mail-4-empty ((,class (:inherit gnus-group-mail-4 :foreground ,comment))) t)
   `(gnus-group-mail-5-empty ((,class (:inherit gnus-group-mail-5 :foreground ,comment))) t)
   `(gnus-group-mail-6-empty ((,class (:inherit gnus-group-mail-6 :foreground ,comment))) t)
   `(gnus-group-news-1 ((,class (:foreground nil :weight normal :inherit outline-5))) t)
   `(gnus-group-news-2 ((,class (:foreground nil :weight normal :inherit outline-6))) t)
   `(gnus-group-news-3 ((,class (:foreground nil :weight normal :inherit outline-7))) t)
   `(gnus-group-news-4 ((,class (:foreground nil :weight normal :inherit outline-8))) t)
   `(gnus-group-news-5 ((,class (:foreground nil :weight normal :inherit outline-1))) t)
   `(gnus-group-news-6 ((,class (:foreground nil :weight normal :inherit outline-2))) t)
   `(gnus-group-news-1-empty ((,class (:inherit gnus-group-news-1 :foreground ,comment))) t)
   `(gnus-group-news-2-empty ((,class (:inherit gnus-group-news-2 :foreground ,comment))) t)
   `(gnus-group-news-3-empty ((,class (:inherit gnus-group-news-3 :foreground ,comment))) t)
   `(gnus-group-news-4-empty ((,class (:inherit gnus-group-news-4 :foreground ,comment))) t)
   `(gnus-group-news-5-empty ((,class (:inherit gnus-group-news-5 :foreground ,comment))) t)
   `(gnus-group-news-6-empty ((,class (:inherit gnus-group-news-6 :foreground ,comment))) t)

   `(erc-direct-msg-face ((,class (:foreground ,orange))) t)
   `(erc-error-face ((,class (:foreground ,red))) t)
   `(erc-header-face ((,class (:foreground ,foreground :background ,selection))) t)
   `(erc-input-face ((,class (:foreground ,green))) t)
   `(erc-keyword-face ((,class (:foreground ,yellow))) t)
   `(erc-current-nick-face ((,class (:foreground ,green))) t)
   `(erc-my-nick-face ((,class (:foreground ,green))) t)
   `(erc-nick-default-face ((,class (:weight normal :foreground ,purple))) t)
   `(erc-nick-msg-face ((,class (:weight normal :foreground ,yellow))) t)
   `(erc-notice-face ((,class (:foreground ,comment))) t)
   `(erc-pal-face ((,class (:foreground ,orange))) t)
   `(erc-prompt-face ((,class (:foreground ,blue))) t)
   `(erc-timestamp-face ((,class (:foreground ,aqua))) t)
   `(erc-keyword-face ((,class (:foreground ,green))) t)

   `(custom-variable-tag ((,class (:foreground ,blue))) t)
   `(custom-group-tag ((,class (:foreground ,blue))) t)
   `(custom-state ((,class (:foreground ,green))) t)

     ;; ansi-term
   `(term ((,class (:foreground nil :background nil :inherit default))) t)
   `(term-color-black   ((,class (:foreground ,foreground :background ,foreground))) t)
   `(term-color-red     ((,class (:foreground ,red :background ,red))) t)
   `(term-color-green   ((,class (:foreground ,green :background ,green))) t)
   `(term-color-yellow  ((,class (:foreground ,yellow :background ,yellow))) t)
   `(term-color-blue    ((,class (:foreground ,blue :background ,blue))) t)
   `(term-color-magenta ((,class (:foreground ,purple :background ,purple))) t)
   `(term-color-cyan    ((,class (:foreground ,aqua :background ,aqua))) t)
   `(term-color-white   ((,class (:foreground ,background :background ,background))))) t)

(dynamic/with-color-variables
 (custom-theme-set-variables
  'dynamic
  `(ansi-color-names-vector (vector ,foreground ,red ,green ,yellow ,blue ,purple ,aqua ,background))
  '(ansi-color-faces-vector [default bold shadow italic underline bold bold-italic bold])))

(provide-theme 'dynamic)

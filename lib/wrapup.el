;;; wrapup -- Wrap up work and the end of the day
;;; Code:
;;; Commentary:

(defvar wrapup_projects
  '("/home/kotfic/org"
    "/home/kotfic/.emacs.d"
    "/home/kotfic/.dot"
    "/home/kotfic/src/email_processor"))

(defun wrapup/next-repo ()
  "Progress to the next repository."
  (interactive)
;  (magit-status p)
;  (delete-other-windows))
  (let ((repo (pop wrapup--wrapup_projects)))
    (if repo
        (progn
          (message repo)
          (magit-status repo)
          (delete-other-windows)
          (wrapup-mode)
          )
      (progn
          (dolist (buffer (buffer-list))
            (with-current-buffer buffer
              (wrapup-mode -1)))))
  ))

(defvar wrapup-mode-keymap (make-keymap) "Wrapup-mode keymap.")
(define-key wrapup-mode-keymap (kbd "C-c n") 'wrapup/next-repo)

(defun wrapup/enter ()
  "Enter the wrapup mode."
  (when (not (boundp 'wrapup--wrapup_projects))
    (setq wrapup--wrapup_projects wrapup_projects)
    (wrapup/next-repo))
  )

(defun wrapup/exit ()
  "Exit wrapup mode."
  (when (boundp 'wrapup--wrapup_projects)
    (makunbound 'wrapup--wrapup_projects))
  )


(define-minor-mode wrapup-mode
  "Documentation"
  :init-value nil
  :lighter " Wrapup"
  :keymap wrapup-mode-keymap
  (if wrapup-mode (wrapup/enter) (wrapup/exit)))

(provide 'wrapup)
;;; wrapup.el ends here

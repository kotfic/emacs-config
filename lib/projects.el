;;; projects -- project specific variables

;;; Commentary:
;; File that contains project specific configurations

;;; Code:

;; We condiser these to be safe!


(defun filter-plist (fn plist)
  (let ((pl plist)
        (vals ()))
    (while pl
      (push (funcall fn (car pl) (cadr pl)) vals)
      (setq pl (cddr pl)))
    (delq nil(nreverse vals))))

(defun ismode? (symbol)
  (or (s-contains? "-mode" (symbol-name symbol))
      (equal ":nil" (symbol-name symbol))))

(defun get-dir-locals (args)
  (filter-plist (lambda (key val)
		  (when (ismode? key)
		    (cons key val))) args))

(defun eval-dir-locals (args)
  ;; for each mode and its var/value list
  (-map (lambda (mode_args)
	  (let ((mode (car mode_args))
		(body (cdr mode_args)))
	    ;; for each var and its value
	    (cons (intern (s-chop-prefix ":" (symbol-name mode)))
		  (-map (lambda (var_val)
			  (let ((var (car var_val))
				(val (cdr var_val)))
			    ;; if first element is 'eval pass through
			    ;; otherwise evaluate the cdr
			    (if (eq var 'eval)
				var_val
			      (cons var (eval val))))
			  ) body))))
	args))

(defmacro defproject(project-name &rest args)
  (declare (indent 1))
  (let* ((project-name-symbol (if (stringp project-name)
                                  (intern project-name)
                                project-name))
	 ))

  `(let* ((project-path ,(plist-get args :path))
	  ,@(plist-get args :vars)
	  (dir-locals (eval-dir-locals
		       (quote ,(get-dir-locals args)))))
     (when dir-locals
       (-map (lambda(class-vars-list)
	       (-map (lambda(class-var)
		       (add-to-list 'safe-local-variable-values class-var))
		     (cdr class-vars-list))) dir-locals)

       (dir-locals-set-class-variables (quote ,project-name)
				       dir-locals))
     (when (file-exists-p project-path)
       (dir-locals-set-directory-class project-path (quote ,project-name))
       ,@(plist-get args :init)
       )))


(defproject minerva-NEX
  :path "/home/kotfic/kitware/projects/NEX/src/OpenGeoscience/minerva/"
  :vars ((base "/home/kotfic/kitware/projects/NEX/src/")
	 (build-dir (concat base "build/girder/"))
	 (girder-dir (concat base "girder/girder/")))
  :nil
  ((projectile-project-test-cmd . (concat "cd " build-dir " && ctest -j8")))
  :js2-mode
  ((flycheck-jshintrc . (concat build-dir "tests/minerva_jshint.cfg"))
   (flycheck-jscsrc . (concat build-dir "tests/minerva_jsstyle.cfg" )))
  :python-mode
  ((eval . (venv-workon "NEX"))
   (flycheck-python-flake8-executable . "/home/kotfic/.venvs/NEX/bin/flake8")
   (flycheck-flake8rc . (concat girder-dir "tests/flake8.cfg"))))


(defproject girder-NEX
  :path "/home/kotfic/kitware/projects/NEX/src/girder/girder/"
  :vars ((base "/home/kotfic/kitware/projects/NEX/src/")
	 (build-dir (concat base "build/girder/"))
	 (girder-dir (concat base "girder/girder/")))
  :nil
  ((projectile-project-test-cmd . (concat "cd " build-dir " && ctest -j8")))
  :js2-mode
  ((flycheck-jshintrc . (concat build-dir "tests/minerva_jshint.cfg"))
   (flycheck-jscsrc . (concat build-dir "tests/minerva_jsstyle.cfg" )))
  :python-mode
  ((eval . (venv-workon "NEX"))
   (flycheck-python-flake8-executable . "/home/kotfic/.venvs/NEX/bin/flake8")
   (flycheck-flake8rc . (concat girder-dir "tests/flake8.cfg")))
  :init

  ((prodigy-define-service
     :name "NEX Girder"
     :command "python"
     :args '("-m" "girder" "-p" "8081")
     :cwd project-path
     :init (lambda ()
	     (venv-workon "NEX"))
     :tags '(nex girder)
     :on-output (lambda (&rest args)
		  (let ((output (plist-get args :output))
			(service (plist-get args :service)))
		    (when (s-matches? "ENGINE Bus STARTED" output)
		      (prodigy-set-status service 'ready)))))

   (prodigy-define-service
     :name "NEX Girder Grunt"
     :command "grunt"
     :args '("watch" "--debug-js")
     :cwd project-path
     :init (lambda ()
	     (venv-workon "NEX"))
     :tags '(nex girder)
     :on-output (lambda (&rest args)
		  (let ((output (plist-get args :output))
			(service (plist-get args :service)))
		    (when (s-matches? "Building JS" output)
		      (prodigy-set-status service 'running))
		    (when (s-matches? "Waiting..." output)
		      (prodigy-set-status service 'ready)))))))


(defproject romanesco
  :path "/home/kotfic/kitware/projects/src/romanesco/"
  :vars ((base "/home/kotfic/kitware/projects/src/")
	 (build-dir (concat base "build/romanesco/")))
  :nil ((eval . (setenv "PYTHONPATH"
			(concat
			 (expand-file-name
			  "/home/kotfic/kitware/projects/src/VTK/build/Wrapping/Python") ":"
			  (expand-file-name
			   "/home/kotfic/kitware/projects/src/VTK/build/lib"))))
	(projectile-project-test-cmd . (concat "cd " build-dir "  && ctest -j8")))
  :python-mode ((eval . (venv-workon "NEX"))
		(flycheck-python-flake8-executable . "/home/kotfic/.venvs/NEX/bin/flake8")
		(flycheck-flake8rc . (concat project-path "setup.cfg"))))


(provide 'projects)
;;; projects.el ends here

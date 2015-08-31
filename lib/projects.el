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
	    (cons mode (-map (lambda (var_val)
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
	  (dir-locals (eval-dir-locals ,(get-dir-locals args))))
     (when dir-locals
       (-map (lambda(class-vars-list)
	       (-map (lambda(class-var)
		       (add-to-list 'safe-local-variable-values class-var))
		     (cdr class-vars-list))) dir-locals)

       (dir-locals-set-class-variables (quote ,project-name)
				       dir-locals)
       (if (file-exists-p project-path)
	   (dir-locals-set-directory-class project-path (quote ,project-name))))))


(mapc (lambda (e)
	(add-to-list 'safe-local-variable-values e))
      '(
	(flycheck-javascript-jshint-executable . "/usr/bin/jshint")
	(flycheck-javascript-jscs-executable . "/usr/bin/jscs")
	(eval . (venv-workon "NEX"))
	(flycheck-python-flake8-executable . "/home/kotfic/.venvs/NEX/bin/flake8")
	(flycheck-flake8rc . "/home/kotfic/kitware/projects/NEX/src/girder/girder/tests/flake8.cfg")
	(projectile-project-test-cmd . "cd /home/kotfic/kitware/projects/NEX/src/build/romanesco  && ctest -j8")
	(projectile-project-test-cmd . "cd /home/kotfic/kitware/projects/NEX/src/build/girder && ctest -j8")
	(projectile-project-test-cmd . "cd /home/kotfic/kitware/projects/src/build/romanesco  && ctest -j8")
	))


;; Project specific classes
(dir-locals-set-class-variables
 'minerva-NEX
 '((nil . ((projectile-project-test-cmd . "cd /home/kotfic/kitware/projects/NEX/src/build/girder && ctest -j8")))
   (js2-mode . ((flycheck-jshintrc . "/home/kotfic/kitware/projects/NEX/src/girder/build/tests/minerva_jshint.cfg")
		(flycheck-jscsrc . "/home/kotfic/kitware/projects/NEX/src/girder/build/tests/minerva_jsstyle.cfg" )
		))
   (python-mode . ((eval . (venv-workon "NEX"))
		   (flycheck-python-flake8-executable . "/home/kotfic/.venvs/NEX/bin/flake8")
		   (flycheck-flake8rc . "/home/kotfic/kitware/projects/NEX/src/girder/girder/tests/flake8.cfg")
		   ))
 ))


(dir-locals-set-class-variables
 'girder-NEX
 '((nil . ((projectile-project-test-cmd . "cd /home/kotfic/kitware/projects/NEX/src/build/girder && ctest -j8")))
   (js2-mode . ((flycheck-jshintrc . "/home/kotfic/kitware/projects/NEX/src/girder/girder/tests/jshint.cfg")
		(flycheck-jscsrc . "/home/kotfic/kitware/projects/NEX/src/girder/build/tests/core_jsstyle.cfg")
		))
   (python-mode . ((eval . (venv-workon "NEX"))
		   (flycheck-python-flake8-executable . "/home/kotfic/.venvs/NEX/bin/flake8")
		   (flycheck-flake8rc . "/home/kotfic/kitware/projects/NEX/src/girder/girder/tests/flake8.cfg")
		   ))
 ))


(dir-locals-set-class-variables
 'romanesco-NEX
 `((nil . ((eval . ,(setenv "PYTHONPATH"
			    (concat
			     (expand-file-name "/home/kotfic/kitware/projects/src/VTK/build/Wrapping/Python") ":"
			     (expand-file-name "/home/kotfic/kitware/projects/src/VTK/build/lib") ":"
			     (getenv "PYTHONPATH"))))
	   (projectile-project-test-cmd . "cd /home/kotfic/kitware/projects/NEX/src/build/romanesco  && ctest -j8")))
   (python-mode . ((eval . ,(venv-workon "NEX"))
		   (flycheck-python-flake8-executable . "/home/kotfic/.venvs/NEX/bin/flake8")
		   (flycheck-flake8rc . ,(concat "/home/kotfic/kitware/projects/NEX/src/romanesco/setup.cfg"))))))


(dir-locals-set-class-variables
 'romanesco
 `((nil . ((eval . ,(setenv "PYTHONPATH"
			    (concat
			     (expand-file-name "/home/kotfic/kitware/projects/src/VTK/build/Wrapping/Python") ":"
			     (expand-file-name "/home/kotfic/kitware/projects/src/VTK/build/lib") ":"
			     (getenv "PYTHONPATH"))))
	   (projectile-project-test-cmd . "cd /home/kotfic/kitware/projects/src/build/romanesco  && ctest -j8")))
   (python-mode . ((eval . ,(venv-workon "NEX"))
		   (flycheck-python-flake8-executable . "/home/kotfic/.venvs/romanesco/bin/flake8")
		   (flycheck-flake8rc . ,(concat "/home/kotfic/kitware/projects/src/romanesco/setup.cfg"))))))



;; Minerva
(when (file-accessible-directory-p
       "/home/kotfic/kitware/projects/NEX/src/OpenGeoscience/minerva/")

  (dir-locals-set-directory-class
   "/home/kotfic/kitware/projects/NEX/src/OpenGeoscience/minerva/"
   'minerva-NEX))

;; Girder
(when (file-accessible-directory-p
       "/home/kotfic/kitware/projects/NEX/src/girder/girder")

  (dir-locals-set-directory-class
   "/home/kotfic/kitware/projects/NEX/src/girder/girder"
   'girder-NEX))


;; Romanesco-NEX
(when (and (file-accessible-directory-p
	    "/home/kotfic/kitware/projects/src/VTK/")
	   (file-accessible-directory-p
	    "/home/kotfic/kitware/projects/NEX/src/romanesco"))

  (dir-locals-set-directory-class
   "/home/kotfic/kitware/projects/NEX/src/romanesco"
   'romanesco-NEX))

(when (and (file-accessible-directory-p
	    "/home/kotfic/kitware/projects/src/VTK/")
	   (file-accessible-directory-p
	    "/home/kotfic/kitware/projects/src/romanesco"))

  (dir-locals-set-directory-class
   "/home/kotfic/kitware/projects/src/romanesco"
   'romanesco))


(provide 'projects)
;;; projects.el ends here

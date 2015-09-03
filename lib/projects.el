;;; projects -- project specific variables

;;; Commentary:
;; File that contains project specific configurations

;;; Code:
(add-to-list 'load-path (concat emacs-config-dir "/lib/defproject"))
(require 'defproject)

(defproject minerva-NEX
  :path "/home/kotfic/kitware/projects/NEX/src/OpenGeoscience/minerva/"
  :vars ((base "/home/kotfic/kitware/projects/NEX/src/")
	 (build-dir (concat base "build/girder/"))
	 (girder-dir (concat base "girder/girder/")))
  :nil
  ((projectile-project-test-cmd . (concat "cd " build-dir " && ctest -j8 -R minerva")))
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
		(flycheck-flake8rc . (concat project-path "tests/flake8.cfg"))))


(provide 'projects)
;;; projects.el ends here

;;; projects -- project specific variables

;;; Commentary:
;; File that contains project specific configurations

;;; Code:

;; We condiser these to be safe!
(mapc (lambda (e)
	(add-to-list 'safe-local-variable-values e))
      '(
	(flycheck-javascript-jshint-executable . "/usr/bin/jshint")
	(flycheck-javascript-jscs-executable . "/usr/bin/jscs")
	(eval . (venv-workon "NEX"))
	(flycheck-python-flake8-executable . "/home/kotfic/.venvs/NEX/bin/flake8")
	(flycheck-flake8rc . "/home/kotfic/kitware/projects/NEX/src/girder/girder/tests/flake8.cfg")
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
		   (flycheck-flake8rc . ,(concat "/home/kotfic/kitware/projects/NEX/src/romanesco" "/setup.cfg"))))))



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


;; Romanesco
(when (and (file-accessible-directory-p
	    "/home/kotfic/kitware/projects/src/VTK/")
	   (file-accessible-directory-p
	    "/home/kotfic/kitware/projects/NEX/src/romanesco"))

  (dir-locals-set-directory-class
   "/home/kotfic/kitware/projects/NEX/src/romanesco"
   'romanesco-NEX))


(provide 'projects)
;;; projects.el ends here

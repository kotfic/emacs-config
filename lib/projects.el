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
 '((js2-mode . ((flycheck-jshintrc . "/home/kotfic/kitware/projects/NEX/src/girder/build/tests/minerva_jshint.cfg")
		(flycheck-jscsrc . "/home/kotfic/kitware/projects/NEX/src/girder/build/tests/minerva_jsstyle.cfg" )
		))
   (python-mode . ((eval . (venv-workon "NEX"))
		   (flycheck-python-flake8-executable . "/home/kotfic/.venvs/NEX/bin/flake8")
		   (flycheck-flake8rc . "/home/kotfic/kitware/projects/NEX/src/girder/girder/tests/flake8.cfg")
		   ))
 ))


(dir-locals-set-class-variables
 'girder-NEX
 '((js2-mode . ((flycheck-jshintrc . "/home/kotfic/kitware/projects/NEX/src/girder/girder/tests/jshint.cfg")
		(flycheck-jscsrc . "/home/kotfic/kitware/projects/NEX/src/girder/build/tests/core_jsstyle.cfg")
		))
   (python-mode . ((eval . (venv-workon "NEX"))
		   (flycheck-python-flake8-executable . "/home/kotfic/.venvs/NEX/bin/flake8")
		   (flycheck-flake8rc . "/home/kotfic/kitware/projects/NEX/src/girder/girder/tests/flake8.cfg")
		   ))
 ))




(when (file-accessible-directory-p
       "/home/kotfic/kitware/projects/NEX/src/OpenGeoscience/minerva/")

  (dir-locals-set-directory-class
   "/home/kotfic/kitware/projects/NEX/src/OpenGeoscience/minerva/"
   'minerva-NEX))


(when (file-accessible-directory-p
       "/home/kotfic/kitware/projects/NEX/src/girder/girder")

  (dir-locals-set-directory-class
   "/home/kotfic/kitware/projects/NEX/src/girder/girder"
   'girder-NEX))



(provide 'projects)
;;; projects.el ends here

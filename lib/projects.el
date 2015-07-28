;;; projects -- project specific variables

;;; Commentary:
;; File that contains project specific configurations

;;; Code:

(add-to-list 'safe-local-variable-values
	     '(flycheck-javascript-jshint-executable . "/usr/bin/jshint"))

(add-to-list 'safe-local-variable-values
	     '(flycheck-javascript-jscs-executable . "/usr/bin/jscs"))

;; Project specific classes
(dir-locals-set-class-variables
 'minerva
 '((js2-mode . ((flycheck-javascript-jshint-executable . "/usr/bin/jshint")
		(flycheck-jshintrc . "/home/kotfic/kitware/projects/NEX/src/girder/build/tests/minerva_jshint.cfg")
		(flycheck-javascript-jscs-executable . "/usr/bin/jscs")
		(flycheck-jscsrc . "/home/kotfic/kitware/projects/NEX/src/girder/build/tests/minerva_jsstyle.cfg" )
		)
	     )))


;; Project locations
(when (file-accessible-directory-p
       "/home/kotfic/kitware/projects/NEX/src/OpenGeoscience/minerva/")

  (dir-locals-set-directory-class
   "/home/kotfic/kitware/projects/NEX/src/OpenGeoscience/minerva/"
   'minerva))



(provide 'projects)
;;; projects.el ends here

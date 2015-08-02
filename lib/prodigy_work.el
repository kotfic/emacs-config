; NEX src base
(let ((base "~/kitware/projects/NEX/src/"))
  (if (file-accessible-directory-p (concat base "girder/girder"))
      (progn

	(prodigy-define-service
	  :name "NEX Romanesco"
	  :command "python"
	  :args '("-m" "romanesco")
	  :cwd (concat base "")
	  :init (lambda ()
		  (venv-workon "NEX"))
	  :tags '(nex romanesco))
	
	(prodigy-define-service
	  :name "NEX GeoJS Examples"
	  :command "grunt"
	  :args '("serve")
	  :cwd (concat base "OpenGeoscience/geojs")
	  :init (lambda ()
		  (venv-workon "NEX"))
	  :tags '(nex geojs))
	
	(prodigy-define-service
	  :name "NEX Girder"
	  :command "python"
	  :args '("-m" "girder")
	  :cwd (concat base "girder/girder")
	  :init (lambda ()
		  (venv-workon "NEX"))
	  :tags '(nex girder))

	(prodigy-define-service
	  :name "NEX Girder Grunt"
	  :command "grunt"
	  :args '("watch" "--debug-js")
	  :cwd (concat base "girder/girder")
	  :init (lambda ()
		  (venv-workon "NEX"))
	  :tags '(nex girder))
	)))

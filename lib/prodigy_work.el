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
	  :tags '(nex geojs)
	  	  :on-output (lambda (&rest args)
		       (let ((output (plist-get args :output))
			     (service (plist-get args :service)))
			 (when (s-matches? "Waiting..." output)
			   (prodigy-set-status service 'ready)))))

	
	(prodigy-define-service
	  :name "NEX Girder"
	  :command "python"
	  :args '("-m" "girder" "-p" "8081")
	  :cwd (concat base "girder/girder")
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
	  :cwd (concat base "girder/girder")
	  :init (lambda ()
		  (venv-workon "NEX"))
	  :tags '(nex girder)
	  :on-output (lambda (&rest args)
		       (let ((output (plist-get args :output))
			     (service (plist-get args :service)))
			 (when (s-matches? "Building JS" output)
			   (prodigy-set-status service 'running))
			 (when (s-matches? "Waiting..." output)
			   (prodigy-set-status service 'ready)))))

	(prodigy-define-service
	  :name "Local Spark Cluster"
	  :command "docker-compose"
	  :args '("up")
	  :cwd (concat base "docker_spark_cluster")
	  :init (lambda ()
		  (venv-workon "NEX"))
	  :tags '(spark)
	  :ready-message "Successfully registered with \\w+ spark://.*:[0-9]\\{4\\}"
	  )
	)))


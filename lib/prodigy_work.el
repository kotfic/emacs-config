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
          :tags '(nex romanesco)
          :on-output (lambda (&rest args)
                       (let ((output (plist-get args :output))
                             (service (plist-get args :service)))
                         (when (s-matches? "WARNING/MainProcess\] celery@.* ready." output)
                           (prodigy-set-status service 'ready)))))

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


;;      (prodigy-define-service
;;        :name "Local Spark Cluster"
;;        :command "docker-compose"
;;        :args '("up")
;;        :cwd (concat base "docker_spark_cluster")
;;        :init (lambda ()
;;                (venv-workon "NEX"))
;;        :tags '(spark)
;;        :ready-message "Successfully registered with \\w+ spark://.*:[0-9]\\{4\\}"
        ;;        )

        )))

(let ((base "~/kitware/projects/NEX/notebooks"))
  (prodigy-define-service
    :name "NEX notebooks"
    :command "ipython"
    :cwd base
    :args '("notebook" "--no-browser")
    :init (lambda () (venv-workon "NEX"))
    :tags '(python_notebook)))

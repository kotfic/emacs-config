
; Utility functions

(defun prodigy-remove-marked-services ()
; TODO this should make sure to end services before removing them see: :process field.
  (interactive)
  (setq prodigy-services 
	(filter (lambda (s) (not (plist-get s :marked))) prodigy-services))
  (prodigy-refresh))

(defun pp:parse_ipython_service_item (item)
  (let ((name (plist-get (plist-get item :notebook) :name))
	(kernel (plist-get (plist-get item :kernel) :id)))		
    (plist-put (plist-put '() :name name) :kernel kernel)))


(defun pp:start-kernel (service-name callback)
  (let ((service (prodigy-find-service service-name)))
    (if service
	(if (prodigy-service-started-p service)
	    (funcall callback)
	  (prodigy-start-service service callback))
      (message (format "Could not find service for %s" pp:service-name)))))


;(defun pp:prodigy-push-connected-buffer (service buffer-name)
;  (when (and buffer-name (get-buffer buffer-name))
;    (plist-put service :connected-buffers (cons (get-buffer buffer-name)
;						(plist-get service :connected-buffers)))))
;


(defun pp:ipython-console-callback (service-name buffer-name)
  "Call run-python in a way that lets us connect to an existing python kernel.
For services that have :kernel defined this connects directly to the kernel id
defined by that property.  For notebooks this prompts with a list of open notebook
kernels defined by the servies' :notebooks property. This is pulled via http request
by the :on-output function defined on the 'python_notebook' prodigy tag."
  (lexical-let ((service-name service-name))    
    #'(lambda ()
	(let ((service (prodigy-find-service service-name)))

	  (cond 
					; Manage Kernels
	   ((and service (plist-get service :kernel))
	    (let* ((python-shell-interpreter-args (format " console --existing %s" (plist-get service :kernel)))
		   (python-shell-buffer-name (format "%s (Console)" service-name)))
	      (run-python (python-shell-parse-command) nil 0)
					; (pp:prodigy-push-connected-buffer service py-buffer-name)
					; (pp:custom-jedi-setup)
	      ))

					; Manage Notebooks
	   ((and service (plist-get service :notebooks))
	    (let* ((name (ido-completing-read "Notebook Kernel:" 
					      (mapcar (lambda (item) (plist-get item :name)) 
						      (plist-get service :notebooks))))
		   (kernel-id (plist-get (-first (lambda (item) 
						   (eq (plist-get item :name) name))
						 (plist-get service :notebooks))
					 :kernel))
		   (python-shell-interpreter-args (format " console --existing %s.json" kernel-id))
		   (python-shell-buffer-name (format "%s (Console)" name)))
	      (run-python (python-shell-parse-command) nil 0)))
	   
	   (t 
	    (message (format "Could not find service/kernel for %s" service-name)))
	   )))))



; Interactive functions

(defun pp:interactively-define-notebook ()
  " Allows for interactively defining a notebook in a particular working directory
This does not take into account virtual environments and simply defines an emacs 
session temporary prodigy service with the prompted working directory with the 
command 'ipython2 notebook' It then starts the service and pops to the prodigy
buffer."
  (interactive)
  (let ((name (read-from-minibuffer "Name: "))
	(directory (ido-read-directory-name "Working Directory: ")))

    (unless (prodigy-find-service name)		    
      (prodigy-define-service
	:name name
	:command "ipython2"
	:args `("notebook")
	:cwd directory
	:tags '(python_notebook)
	:stop-signal 'sigquit ))
    
    (prodigy-start-service (prodigy-find-service name))
    (prodigy)))


; TODO pop to buffer if already connected

(defun pp:ipython-connect () 
  "Start a kernel or notebook and then generate a console-connect function to pass 
as a callback to prodigy."
  (interactive)
  (let* ((service-name (or (and (boundp 'service-name) service-name)
			   (ido-completing-read "Service:" 
						(mapcar (lambda (s) (plist-get s :name)) 
							(append (prodigy-services-tagged-with 'python_kernel) 
								(prodigy-services-tagged-with 'python_notebook))))))
	 (buffer-name (or (plist-get (prodigy-find-service service-name) :console-buffer-name)
			  (concat "*" (plist-get (prodigy-find-service service-name) :name) " (Console)*"))))
    (pp:start-kernel service-name (pp:ipython-console-callback service-name buffer-name))))



(defun pp:py-execute () nil)



; Prodigy objects and infrastructure

(prodigy-define-tag
  :name 'python_kernel
  :on-output (lambda (&rest args)
	       (let ((output (plist-get args :output))
		     (service (plist-get args :service)))
		 (cond ((s-matches? "--existing kernel-\\(.*\\).json$" output)
			(let ((id (cadr (s-match "--existing kernel-\\(.*\\).json$" output))))
			  (plist-put service :kernel (format "kernel-%s.json" id))
			  (prodigy-set-status service 'ready)))))))



(prodigy-define-tag 
  :name 'python_cluster
  :on-output (lambda (&rest args)
	       (let ((output (plist-get args :output))
		     (service (plist-get args :service)))
		 (cond ((s-matches? "Engines appear to have started successfully" output)
			(prodigy-set-status service 'ready))
		       ((s-matches? "Starting 4 Engines with LocalEngineSetLauncher" output)
			(prodigy-set-status service 'starting))))))



(prodigy-define-tag
  :name 'python_notebook
  :on-output (lambda (&rest args)
	       (let ((output (plist-get args :output))
		     (service (plist-get args :service)))
		 (cond ((s-matches? "Notebook is running at: .*?://\\(.*\\):\\([0-9]+\\)" output)
			(let ((url (cadr (s-match "Notebook is running at: \\(.*?://.*:[0-9]+\\)" output)))
			      (host (cadr (s-match "Notebook is running at: .*?://\\(.*\\):\\([0-9]+\\)" output)))
			      (port (caddr (s-match "Notebook is running at: .*?://\\(.*\\):\\([0-9]+\\)" output))))
			  (plist-put service :url url)
			  (plist-put service :host host)
			  (plist-put service :port port)
			  (prodigy-set-status service 'running)))
		       ((or (s-matches? "Kernel started: \\(.*\\)$" output) (s-matches? "Kernel shutdown:  \\(.*\\)$" output) )
			(lexical-let ((service-name (plist-get service :name)))
			  (let ((id (cadr (s-match "Kernel started: \\(.*\\)$" output))))
			    (request 
			     (concat (plist-get service :url) "/api/sessions")
			     :type "GET"
			     :parser (lambda () (let ((json-object-type 'plist)) (json-read)))
			     :success (function*
				       (lambda (&key data &allow-other-keys)
					 (plist-put (prodigy-find-service service-name) :notebooks (mapcar 'pp:parse_ipython_service_item data))))))
			  (prodigy-set-status service 'connected)))))))




(prodigy-define-status :id 'connected :face 'prodigy-green-face)
(prodigy-define-status :id 'ready :face 'prodigy-green-face)
(prodigy-define-status :id 'starting :face 'prodigy-yellow-face)


(define-key prodigy-mode-map "d" 'prodigy-remove-marked-services)


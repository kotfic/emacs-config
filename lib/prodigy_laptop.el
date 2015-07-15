
;; Define and load prodigy services related to my laptop

(if (file-accessible-directory-p "~/notebooks/")
    (prodigy-define-service
      :name "Personal Notebooks"
      :command "ipython"
      :args '("notebook")
      :cwd "~/notebooks/"
      :tags '(python_notebook)))

(if (file-executable-p "~/.pyenv/shims/elfeed_wrapper")
    (prodigy-define-service
      :name "Elfeed Wrapper"
      :command "elfeed_wrapper"
      :cwd "~/"))

(if (file-executable-p "/usr/bin/offlineimap")
    (prodigy-define-service
      :name "Offline IMAP"
      :command "/usr/bin/offlineimap"
      :cwd "~/"))


(defhydra hydra-comps-tagger
  (:color pink
	  :post hydra-org-tagger/body)
  "Comps tagger"
  ("0" (org-toggle-tag "cinf720") "cinf72(0)")
  ("1" (org-toggle-tag "cinf721") "cinf72(1)")
  ("2" (org-toggle-tag "cinf722") "cinf72(2)")
  ("3" (org-toggle-tag "cinf723") "cinf72(3)")
  ("4" (org-toggle-tag "cinf724") "cinf72(4)")
  ("g" (org-toggle-tag "cgog500") "c(g)og500")
  ("f" (org-toggle-tag "cframework") "c(f)ramework")
  ("c" (org-toggle-tag "comps") "(c)omps")
  ("q" (org-toggle-tag "cquote") "c(q)uote")

  ("<return>" nil "quit" :exit t))

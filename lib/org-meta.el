(defhydra hydra-grant-tagger
  (:color pink
	  :post hydra-org-tagger/body)
  "Grants tagger"
  ("t" (org-toggle-tag "term") "(t)erm")
  ("p" (org-toggle-tag "paper") "(p)aper")
  ("q" (org-toggle-tag "question") "(q)uestion")
  ("s" (org-toggle-tag "story") "(s)tory")
  ("f" (org-toggle-tag "followup") "(f)ollowup")
  ("<return>" nil "quit" :exit t))


(define-key
  org-mode-map
  (kbd "C-t")
  (defhydra hydra-org-tagger
    (:color blue)
    "Org-mode tagger"
    ("g" hydra-grant-tagger/body "(g)rants" :color blue)
    ("<return>" nil "quit" :exit t)))

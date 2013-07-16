(TeX-add-style-hook "bare_conf"
 (lambda ()
    (LaTeX-add-bibliographies
     "biblio")
    (LaTeX-add-environments
     "mydef")
    (LaTeX-add-labels
     "sec:intro"
     "sec:prev-work"
     "sec:defs"
     "fig_first_case"
     "fig_second_case"
     "fig_sim"
     "sec:dls"
     "sec:aco"
     "sec:experiments"
     "sec:conclusions")
    (TeX-run-style-hooks
     "caption"
     "graphicx"
     "subfig"
     "lofdepth"
     "lotdepth"
     "fontenc"
     "T1"
     "amsthm"
     "inputenc"
     "utf8"
     "latex2e"
     "IEEEtran10"
     "IEEEtran"
     "conference")))


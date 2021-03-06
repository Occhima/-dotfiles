;; -*- no-byte-compile: t; -*-
;;; $DOOMDIR/packages.el

;; To install a package with Doom you must declare them here and run 'doom sync'
;; on the command line, then restart Emacs for the changes to take effect -- or
;; use 'M-x doom/reload'.

;; To install SOME-PACKAGE from MELPA, ELPA or emacsmirror:
;(package! some-package)
;;; Packages:
(package! org-super-agenda)
;; (package! wakatime-mode)
;; (package! bug-hunter)
;; (package! org-roam-bibtex
;;   :recipe (:host github :repo "org-roam/org-roam-bibtex"))

;; When using org-roam via the `+roam` flag
(unpin! org-roam company-org-roam)
;; (unpin! bibtex-completion helm-bibtex ivy-bibtex)
(package! sql-indent)
(package! sqlformat)
(package! org-modern)
(package! notebook-mode)
(package! telega)
;; (package! vue-mode)
;; (package! vue-html-mode)

(package! notebook-mode
  :recipe (:host github :repo "rougier/notebook-mode"))

;; (package! mu4e-dashboard
;;   :recipe (:host github :repo "rougier/mu4e-dashboard"))

(package! pdf-tools)
(package! org-pdftools
  :recipe (:host github :repo "fuxialexander/org-pdftools"))
;; (package! org-ref
;;   :recipe (:host github :repo "jkitchin/org-ref"))
(package! org-noter
  :recipe (:host github :repo "weirdNox/org-noter"))
(package! org-noter-pdftools
  :recipe (:host github :repo "fuxialexander/org-pdftools"))

;; To install a package directly from a remote git repo, you must specify a
;; `:recipe'. You'll find documentation on what `:recipe' accepts here:
;; https://github.com/raxod502/straight.el#the-recipe-format
;(package! another-package
;  :recipe (:host github :repo "username/repo"))

  ; :recipe (:host github :repo "alphapapa/org-ql"))

;; (straight-use-package
;;  '(webkit :type git :host github :repo "akirakyle/emacs-webkit"
;;           :branch "main"
;;           :files (:defaults "*.js" "*.css")
;;           :build ("make")))

; (straight-use-package '(ox-html :type built-in))
; (package! ox-tailwind
;   :recipe (:host github :repo "vascoferreira25/ox-tailwind"))

;; If the package you are trying to install does not contain a PACKAGENAME.el
;; file, or is located in a subdirectory of the repo, you'll need to specify
;; `:files' in the `:recipe':
;(package! this-package
;  :recipe (:host github :repo "username/repo"
;           :files ("some-file.el" "src/lisp/*.el")))

;; If you'd like to disable a package included with Doom, you can do so here
;; with the `:disable' property:
;(package! builtin-package :disable t)

;; You can override the recipe of a built in package without having to specify
;; all the properties for `:recipe'. These will inherit the rest of its recipe
;; from Doom or MELPA/ELPA/Emacsmirror:
;(package! builtin-package :recipe (:nonrecursive t))
;(package! builtin-package-2 :recipe (:repo "myfork/package"))

;; Specify a `:branch' to install a package from a particular branch or tag.
;; This is required for some packages whose default branch isn't 'master' (which
;; our package manager can't deal with; see raxod502/straight.el#279)
;(package! builtin-package :recipe (:branch "develop"))

;; Use `:pin' to specify a particular commit to install.
;(package! builtin-package :pin "1a2b3c4d5e")

(package! nvm)
(package! tree-sitter)
(package! tree-sitter-langs)

;; (package! lsp-python-ms)
;; (package! org-brain)
;; (package! webpaste)
;; (package! poly-markdown)
;; (package! helm-swoop) 
;; (package! crux)
;; (package! emms) 
;; (package! elfeed)
;; (package! elfeed-goodies)
;; (package! org-trello)

;; (package! org-fragtog)

(unpin! org-roam)
;; (package! org-roam-ui)
(package! beacon)
;; (package! clippy)

;; (package! org-krita
;;   :recipe (:host github
;;            :repo "lepisma/org-krita"
;;            :files ("resources" "resources" "*.el" "*.el")))

(package! py-pyment :recipe (:host github :repo "humitos/py-cmd-buffer.el"))
(package! buftra :recipe (:host github :repo "humitos/buftra.el"))  ;; py-pyment dependency!

(package! polymode :recipe
  (:host github :repo "polymode/polymode"))
(package! poly-markdown :recipe
  (:host github :repo "polymode/poly-markdown"))
(package! poly-R :recipe
  (:host github :repo "polymode/poly-R"))
(package! poly-org :recipe
  (:host github :repo "polymode/poly-org"))
;; (package! poly-rst :recipe
;;   (:host github :repo "polymode/poly-rst"))
(package! poly-noweb :recipe
  (:host github :repo "polymode/poly-noweb"))

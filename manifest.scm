;; manifest.scm
(use-modules (guix profiles))
(packages->manifest (map specification->package
                         '("emacs-pgtk" ;Emacs itself
                           "emacs-use-package" ;use-package macro (no auto-install)
                           "emacs-org" ;org (usually bundled, included for clarity)
                           "emacs-org-roam"
                           "emacs-citar-org-roam" ;integration of citar into org-roam (provides citar)
                           )))

;; manifest.scm
(use-modules (guix profiles))
(packages->manifest (map specification->package
                         '("emacs-pgtk" ;Emacs itself
                           "emacs-use-package" ;use-package macro (no auto-install)
                           "emacs-org" ;org (usually bundled, included for clarity)
                           "emacs-org-roam"
                           "emacs-org-roam-ui" ;web view for org-roam
                           "emacs-citar-org-roam" ;integration of citar into org-roam (provides citar)
                           "emacs-org-noter" ;ebook annotator
                           "emacs-pdf-tools" ;on-demand pdf rendering
                           "emacs-org-pdftools" ;support for org links from pdftools buffers
                           "emacs-nov-el" ;major mode for reading EPUB documents
                           )))

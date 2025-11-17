;;; ── Guix-only stance + core defaults (use-package emacs) ───────────────────
(eval-when-compile (require 'use-package))
(setq use-package-always-ensure nil) ; never auto-install; Guix provides packages

(use-package emacs
  :ensure nil
  :bind (("C-x C-b" . ibuffer))        ; global remap
  :hook (text-mode . visual-line-mode)  ; covers rst/markdown/etc.
  :init
  ;; Don’t enable ELPA at startup; we use Guix packages only.
  (setq package-enable-at-startup nil
        package-archives nil
        use-package-always-ensure nil)
  (setq inhibit-startup-screen t
        ring-bell-function #'ignore
        use-short-answers t)
  ;; history configs
  (setq history-length 500
        history-delete-duplicates t
        savehist-file (locate-user-emacs-file "var/savehist")
        savehist-autosave-interval 300) ; seconds
  ;; Base directories for backups and auto-saves
  (defvar zie/emacs-backup-dir (expand-file-name "backup/" user-emacs-directory))
  (defvar zie/emacs-auto-save-dir (expand-file-name "auto-save/" user-emacs-directory))
  (make-directory zie/emacs-backup-dir t)
  (make-directory zie/emacs-auto-save-dir t)

  ;; 1) Normal backup files: foo → foo~
  (setq backup-directory-alist `(("." . ,zie/emacs-backup-dir)))

  ;; 2) Auto-save files: #foo# etc.
  (setq auto-save-file-name-transforms `((".*" ,zie/emacs-auto-save-dir t)))

  ;; 3) Auto-save list (the .saves-* files)
  (setq auto-save-list-file-prefix (expand-file-name ".saves-" zie/emacs-auto-save-dir))

  ;; Optional tuning of backup behavior
  (setq make-backup-files t           ; keep making backups
        backup-by-copying t           ; avoid messing with symlinks
        delete-old-versions t
        kept-new-versions 6
        kept-old-versions 2
        version-control t)            ; numbered backups: file.~1~, file.~2~, ...
  :custom
  (set-face-attribute 'default nil :family "AdwaitaMono Nerd Font" :height 90)
  :config
  ;; post-startup toggles
  (menu-bar-mode -1)
  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (savehist-mode 1)
  ;; Persist additional non-minibuffer rings (useful extras)
  (dolist (var '(search-ring
                 regexp-search-ring
                 compile-command)) ; last compile recipe
    (add-to-list 'savehist-additional-variables var))
  (defun zie/cleanup-emacs-temp-files (dir)
    "Delete Emacs backup (~) and auto-save (#...#) files under DIR."
    (interactive "Directory to clean: ")
    (let ((patterns '("~\\'" "\\`#.*#\\'")))
      (dolist (pattern patterns)
        (dolist (file (directory-files-recursively dir pattern))
          (ignore-errors (delete-file file)))))
    (message "Emacs temp files cleaned in %s" dir)))

;;;; ── Org basics ────────────────────────────────────────────────────────────
(use-package org
  :ensure nil
  :hook (org-mode . visual-line-mode))   ; Org isn't derived from text-mode so it needs its own

;; Optional but handy: make Org links prefer IDs when available
(use-package org-id :ensure nil :config (setq org-id-link-to-org-use-id t))

;;;; ── Org-babel: PlantUML ───────────────────────────────────────────────────
(use-package ob-plantuml
  :ensure nil           ;; comes with Org / Emacs, Guix provides Org
  :after org
  :config
  ;; Use the 'plantuml' executable from PATH (Guix package)
  (setq org-plantuml-exec-mode 'plantuml)
  ;; If you prefer the .jar instead, use:
  ;; (setq org-plantuml-exec-mode 'jar
  ;;       org-plantuml-jar-path "/path/to/plantuml.jar")
    ;; Optional: default header args
  (setq org-babel-default-header-args:plantuml
        '((:results . "file")
          (:exports . "results")
          (:mkdirp . "yes")))
  ;; Ensure output directory for :file exists (honors :mkdirp when present)
  (defun zie/org-babel-plantuml-mkdirp (orig-fun body params)
    (let* ((file   (cdr (assq :file params)))
           (dir    (and file (file-name-directory file)))
           (mkdirp (cdr (assq :mkdirp params))))
      (when (and dir
                 (not (string= dir ""))
                 mkdirp                     ;; any non-nil value
                 (not (file-exists-p dir)))
        (make-directory dir t))
      (funcall orig-fun body params)))

  (advice-add 'org-babel-execute:plantuml :around
              #'zie/org-babel-plantuml-mkdirp)
  ;; Enable PlantUML in org-babel
  (add-to-list 'org-babel-load-languages '(plantuml . t))
  (org-babel-do-load-languages 'org-babel-load-languages
                               org-babel-load-languages))
;;;; ── Org-roam ──────────────────────────────────────────────────────────────
(use-package
 org-roam
 :ensure nil
 :init
 ;; Base directories
 (setq org-roam-directory (expand-file-name "~/Notes")) (setq org-roam-dailies-directory "daily/")
 :bind
 (("C-c n f" . org-roam-node-find) ;; find/create note
  ("C-c n i" . org-roam-node-insert) ;; insert link / create on the fly
  ("C-c n b" . org-roam-buffer-toggle) ;; backlinks side panel
  ("C-c n t" . org-roam-dailies-capture-today)
  ("C-c n y" . org-roam-dailies-capture-yesterday)
  ("C-c n m" . org-roam-dailies-capture-tomorrow))
 :config
 ;; Ensure subfolders exist (one-time, harmless if already present)
 (dolist
  (dir '("concepts" "permanent" "projects" "meta" "daily" "references"))
  (make-directory (expand-file-name (concat dir "/") org-roam-directory) t))

 ;; Capture templates
 (setq
  org-roam-capture-templates
  '(
    ;; Concept — one idea/term per note
    ("c"
     "Concept"
     plain
     "* Definition\n\n%?\n\n* Notes\n- \n\n* Links\n- "
     :if-new (file+head "concepts/%<%Y%m%d%H%M>--${slug}.org" "#+title: ${title}\n")
     :unnarrowed t)

    ;; Permanent — your synthesis/insight
    ("p"
     "Permanent"
     plain
     "* Claim\n\n%?\n\n* Why it matters\n- \n\n* Evidence / Sources\n- \n\n* Related\n- "
     :if-new (file+head "permanent/%<%Y%m%d%H%M>--${slug}.org" "#+title: ${title}\n")
     :unnarrowed t)

    ;; Project — goal/TODO hub that links out
    ("P"
     "Project"
     plain
     "* Goals\n- \n\n* TODOs\n- [ ] \n\n* Notes\n- \n\n* Related\n- "
     :if-new (file+head "projects/%<%Y%m%d%H%M>--${slug}.org" "#+title: ${title}\n")
     :unnarrowed t)
    ("m"
     "Meta (MOC)"
     plain
     "* Scope\n%?\n\n* Concepts\n- \n\n* Permanents\n- \n\n* Related hubs\n- \n\n* Open questions\n- \n"
     :if-new (file+head "meta/%<%Y%m%d%H%M>--${slug}.org" "#+title: ${title}\n#+filetags: :moc:\n")
     :unnarrowed t)
    ("M"
     "Meta (Area Overview)"
     plain
     "#+title: ${title}\n#+filetags: :moc:\n\n* Overview\n%?\n\n* Index\n| Type      | Link |\n|-----------+------|\n| Concept   |      |\n| Permanent |      |\n\n* Concepts\n- \n\n* Permanents\n- \n\n* Related hubs\n- \n\n* Notes\n- \n"
     :if-new (file+head "meta/%<%Y%m%d%H%M>--${slug}.org" "")
     :unnarrowed t)

    ;; literature / reference note, used by citar-org-roam
    ("r"
     "Reference (literature note)"
     plain
     "* Summary\n\n%?\n\n* Key ideas\n- \n\n* Quotes\n- \n\n* Links\n- \n"
     :target
     (file+head
      "%(expand-file-name (or citar-org-roam-subdir \"references\") org-roam-directory)/${citar-citekey}.org"
      "#+title: ${note-title} (${citar-date})\n#+filetags: :reference:\n#+created: %U\n#+last_modified: %U\n\n")
     :unnarrowed t)

    ("t" "conference talk" plain "%?"
     :if-new
     (file+head
      "talks/%<%Y%m%d%H%M>--${slug}.org"
      "#+title: ${title}\n\
#+filetags: :talk:cpp:\n\
:PROPERTIES:\n\
:ROAM_REFS: %^{Video or slides URL}\n\
:EVENT: %^{Event|CppCon|CppNow|Meeting C++|Other}\n\
:SPEAKER: %^{Speaker}\n\
:YEAR: %^{Year}\n\
:TRACK: %^{Track|Core C++|Performance|Tooling|Other}\n\
:END:\n\
\n* Summary\n- \n\n* Key ideas\n- \n\n* Notes\n- \n\n* Open questions\n- \n\n* Related notes\n- \n")
     :unnarrowed t)))

 ;; Dailies
 (setq
  org-roam-dailies-capture-templates
  '(("d"
     "Default"
     entry
     "* %<%H:%M> %?\n"
     :if-new (file+head "%<%Y-%m-%d>.org" "#+title: %<%Y-%m-%d>\n"))))

 ;; Keep the database synced automatically
 (org-roam-db-autosync-mode 1))

(use-package org-roam-ui :ensure nil :after org-roam :custom (org-roam-ui-sync-theme t))

;; ── Bibliography / citations (org-cite + citar) ─────────────
(use-package
 citar
 :ensure nil ; installed via Guix
 :custom (org-cite-global-bibliography '("~/Notes/references/references.bib"))
 ;; tell org-cite to use citar
 (org-cite-insert-processor 'citar)
 (org-cite-follow-processor 'citar)
 (org-cite-activate-processor 'citar)
 ;; citar reads the same bibliography list
 (citar-bibliography org-cite-global-bibliography)
 :hook
 ;; completion-at-point for citations in Org
 (org-mode . citar-capf-setup)
 :bind
 ;; shorter key to insert citations (in addition to C-c C-x C-@)
 (:map org-mode-map ("C-c b" . org-cite-insert)))

;; ── Citar ↔ Org-roam integration ────────────────────────────
(use-package
 citar-org-roam
 :ensure nil
 :after (citar org-roam)
 :custom
 ;; How titles of roam ref-notes appear in the org-roam database
 (citar-org-roam-note-title-template "${author} - ${title}")
 ;; Subdirectory under org-roam-directory for literature notes
 ;; -> ~/Notes/references/
 (citar-org-roam-subdir "references")
 ;; Which org-roam capture template to use for new literature notes
 ;; (we'll define template key \"r\" below)
 (citar-org-roam-capture-template-key "r")
 :config (citar-org-roam-mode 1))

;;;; ── PDF tools ──────────────────────────────────────────────
(use-package
 pdf-tools
 :ensure nil
 :magic ("%PDF" . pdf-view-mode)
 :config
 ;; Setup pdf-tools; after first install you can also run M-x pdf-tools-install
 (pdf-tools-install))

;;;; ── EPUB tools ─────────────────────────────────────────────
(use-package nov :ensure nil :mode ("\\.epub\\'" . nov-mode))

;;;; ── Org-noter ──────────────────────────────────────────────
(use-package
 org-noter
 :ensure nil
 :commands (org-noter)
 :after (org pdf-tools org-roam)
 :custom
 ;; Where org-noter looks for notes when starting from a PDF
 ;; Adjust if you use another folder for literature notes
 (org-noter-notes-search-path (list (expand-file-name "references" org-roam-directory)))
 ;; I’d keep everything in the same frame
 (org-noter-always-create-frame nil)
 :config
 ;; Enable org-roam integration provided by org-noterg
 (org-noter-enable-org-roam-integration))

;;;; ── Org ↔ pdf-tools integration ───────────────────────────
;; workaround for Emacs 30 
(require 'cl-lib)
(defalias 'find-if 'cl-find-if)
(defalias 'getf 'cl-getf)
(defalias 'equalp 'cl-equalp)

(use-package
 org-pdftools
 :ensure nil
 :after (org pdf-tools)
 :hook (org-mode . org-pdftools-setup-link))

;;;; ── Guix-only stance ──────────────────────────────────────────────────────
(setq package-enable-at-startup nil)     ; don't autoload ELPA
(setq package-archives nil)              ; no remote archives
(setq use-package-always-ensure nil)     ; never auto-install
(eval-when-compile (require 'use-package))

;; Base directories
(defvar my/emacs-backup-dir (expand-file-name "backup/" user-emacs-directory))
(defvar my/emacs-auto-save-dir (expand-file-name "auto-save/" user-emacs-directory))

(make-directory my/emacs-backup-dir t)
(make-directory my/emacs-auto-save-dir t)

;; 1) Normal backup files: foo → foo~
(setq backup-directory-alist
      `(("." . ,my/emacs-backup-dir)))

;; 2) Auto-save files: #foo# etc.
(setq auto-save-file-name-transforms
      `((".*" ,my/emacs-auto-save-dir t)))

;; 3) Auto-save list (the .saves-* files)
(setq auto-save-list-file-prefix
      (expand-file-name ".saves-" my/emacs-auto-save-dir))

;; Optional tuning of backup behavior
(setq make-backup-files t          ; keep making backups
      backup-by-copying t          ; avoid messing with symlinks
      delete-old-versions t
      kept-new-versions 6
      kept-old-versions 2
      version-control t)           ; numbered backups: file.~1~, file.~2~, ...


(defun zie/cleanup-emacs-temp-files (dir)
  "Delete Emacs backup (~) and auto-save (#...#) files under DIR."
  (interactive "DDirectory to clean: ")
  (let ((patterns '("~\\'" "\\`#.*#\\'")))
    (dolist (pattern patterns)
      (dolist (file (directory-files-recursively dir pattern))
        (ignore-errors
          (delete-file file)))))
  (message "Emacs temp files cleaned in %s" dir))

;;;; ── Org basics ────────────────────────────────────────────────────────────
(use-package org :ensure nil)

;; Optional but handy: make Org links prefer IDs when available
(use-package org-id
  :ensure nil
  :config
  (setq org-id-link-to-org-use-id t))
;;;; ── Org-roam ──────────────────────────────────────────────────────────────
(use-package org-roam
  :ensure nil
  :init
  ;; Base directories
  (setq org-roam-directory (expand-file-name "~/Notes"))
  (setq org-roam-dailies-directory "daily/")
  :bind
  (("C-c n f" . org-roam-node-find)            ;; find/create note
   ("C-c n i" . org-roam-node-insert)          ;; insert link / create on the fly
   ("C-c n b" . org-roam-buffer-toggle)        ;; backlinks side panel
   ("C-c n t" . org-roam-dailies-capture-today)
   ("C-c n y" . org-roam-dailies-capture-yesterday)
   ("C-c n m" . org-roam-dailies-capture-tomorrow))
  :config
  ;; Ensure subfolders exist (one-time, harmless if already present)
  (dolist (dir '("concepts" "permanent" "projects" "meta" "daily"))
    (make-directory (expand-file-name (concat dir "/") org-roam-directory) t))

  ;; Capture templates
  (setq org-roam-capture-templates
        '(
          ;; Concept — one idea/term per note
          ("c" "Concept" plain
           "* Definition\n\n%?\n\n* Notes\n- \n\n* Links\n- "
           :if-new (file+head "concepts/%<%Y%m%d%H%M>--${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)

          ;; Permanent — your synthesis/insight
          ("p" "Permanent" plain
           "* Claim\n\n%?\n\n* Why it matters\n- \n\n* Evidence / Sources\n- \n\n* Related\n- "
           :if-new (file+head "permanent/%<%Y%m%d%H%M>--${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)

          ;; Project — goal/TODO hub that links out
          ("P" "Project" plain
           "* Goals\n- \n\n* TODOs\n- [ ] \n\n* Notes\n- \n\n* Related\n- "
           :if-new (file+head "projects/%<%Y%m%d%H%M>--${slug}.org"
                              "#+title: ${title}\n")
           :unnarrowed t)
          ("m" "Meta (MOC)" plain
           "* Scope\n%?\n\n* Concepts\n- \n\n* Permanents\n- \n\n* Related hubs\n- \n\n* Open questions\n- \n"
           :if-new (file+head "meta/%<%Y%m%d%H%M>--${slug}.org"
                              "#+title: ${title}\n#+filetags: :moc:\n")
           :unnarrowed t)
         ("M" "Meta (Area Overview)" plain
          "#+title: ${title}\n#+filetags: :moc:\n\n* Overview\n%?\n\n* Index\n| Type      | Link |\n|-----------+------|\n| Concept   |      |\n| Permanent |      |\n\n* Concepts\n- \n\n* Permanents\n- \n\n* Related hubs\n- \n\n* Notes\n- \n"
          :if-new (file+head "meta/%<%Y%m%d%H%M>--${slug}.org" "")
          :unnarrowed t)))

  ;; Dailies
  (setq org-roam-dailies-capture-templates
        '(("d" "Default" entry
           "* %<%H:%M> %?\n"
           :if-new (file+head "%<%Y-%m-%d>.org"
                              "#+title: %<%Y-%m-%d>\n"))))

  ;; Keep the database synced automatically
  (org-roam-db-autosync-mode 1))

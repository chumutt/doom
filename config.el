;;; $DOOMDIR/config.el -*- lexical-binding: t; -*-

(setq user-full-name "chu the pup"
      user-mail-address "chufilthymutt@gmail.com")

(setq inhibit-x-resources t) ; inhibit .xresources file from being loaded on emacs init

(set-frame-parameter (selected-frame) 'alpha 100)

(add-to-list 'default-frame-alist '(alpha 100))

(set-frame-parameter nil 'alpha-background 100)

(add-to-list 'default-frame-alist '(alpha-background . 100))

(setq doom-font (font-spec :family "Mono" :size 12))

(setq image-use-external-converter t)

(require 'random-splash-image)

(setq random-splash-image-dir
      (concat
       (getenv "HOME")
       "/.local/share/random-splash-image-dir/chosen-splash-images/src/"))

(with-eval-after-load 'random-splash-image
  (random-splash-image-set))

(defun toggle-transparency ()
  "Toggle TOTAL EMACS X11 transparency. Might need to be called a couple of times in a row to work."
  (interactive)
  (let ((alpha (frame-parameter nil 'alpha)))
    (if (eq
     (if (numberp alpha)
         alpha
       (cdr alpha)) ; may also be nil
     100)
    (set-frame-parameter nil 'alpha '(85 . 50))
      (set-frame-parameter nil 'alpha '(100 . 100)))))

(defun toggle-background-transparency ()
  "Toggle background transparency, wherein text and other elements in frame are still displayed but a background isn't."
  (interactive)
  (if (get 'toggle-background-transparency 'state)
      (progn
        (set-frame-parameter nil 'alpha-background 100)
        (put 'toggle-background-transparency 'state nil))
    (progn
      (set-frame-parameter nil 'alpha-background 75)
      (put 'toggle-background-transparency 'state t))))

(after! org-mode
  (setq org-log-into-drawer "LOGBOOK"))

(after! org-auto-tangle-mode
  (add-hook 'org-mode-hook 'org-auto-tangle-mode))

(add-hook 'org-mode-hook 'org-fragtog-mode)

(setq org-todo-keywords
       '((sequence "TODO(t)" "PROJ(p)" "LOOP(r)" "STRT(s)" "WAIT(w)" "HOLD(h)" "HABIT(H)" "IDEA(i)" "|" "DONE(d)" "KILL(k)")
         (sequence "[ ](T)" "[-](S)" "[?](W)" "|" "[X](D)")
         (sequence "|" "OKAY(o)" "YES(y)" "NO(n)")))

(defun ap/org-fix-blank-lines (prefix)
  "Fix blank lines (or lack thereof) between entries and between planning-lines/drawers and entry contents in current subtree.
    With prefix, operate on whole buffer."
  (interactive "P")
  (save-excursion
    (when prefix
      (goto-char (point-min)))
    (when (org-before-first-heading-p)
      (outline-next-heading))
    (ap/org-fix-blank-lines-between-subtree-nodes)
    (ap/org-fix-blank-lines-after-headings)))

(defun ap/org-fix-blank-lines-between-subtree-nodes ()
  "Make sure each heading in subtree is separated from the previous heading's content by a blank line."
  (interactive)
  (save-excursion
    (unless (org-at-heading-p)
      (org-back-to-heading))
    (ignore-errors
      ;; Try to work on the parent-level heading to fix all siblings
      ;; of current heading, but if we're at a top-level heading,
      ;; ignore the error.
      (outline-up-heading 1))
    (let ((m (point-marker)))
      (cl-flet ((fix nil (while (not (looking-back "\n\n"))
                           (insert-before-markers "\n"))))
        (org-map-entries #'fix t 'tree))
      ;; Inserting blank lines may move the point, depending on whether
      ;; it was at the beginning of a heading line or somewhere else.
      ;; Use the marker to make sure we are at the same position.
      (goto-char m)
      (org-with-wide-buffer
       ;; `org-map-entries' narrows the buffer, so `looking-back'
       ;; can't see newlines before the top heading, which may cause
       ;; extra newlines to be inserted.  Now we clean them up.
       (outline-back-to-heading)
       (while (looking-back (rx (>= 3 "\n")))
         (delete-char -1 nil)))
      (set-marker m nil))))

(defun ap/org-fix-blank-lines-after-headings ()
  "Make sure a blank line exists after a heading's drawers and planning lines, before the entry content."
  (interactive)
  (when (org-before-first-heading-p)
    (user-error "Before first heading."))
  (cl-flet ((fix nil (let ((end (org-entry-end-position)))
                       (forward-line)
                       (while (and (org-at-planning-p)
                                   (< (point) (point-max)))
                         ;; Skip planning lines
                         (forward-line))
                       (while (re-search-forward org-drawer-regexp end t)
                         ;; Skip drawers.  You might think that
                         ;; `org-at-drawer-p' would suffice, but for
                         ;; some reason it doesn't work correctly
                         ;; when operating on hidden text.  This
                         ;; works, taken from
                         ;; `org-agenda-get-some-entry-text'.
                         (re-search-forward "^[ \t]*:END:.*\n?" end t)
                         (goto-char (match-end 0)))
                       (unless (or (= (point) (point-max))
                                   (org-at-heading-p)
                                   (looking-at-p "\n"))
                         (insert "\n")))))
    (org-map-entries #'fix t 'tree)))

(after! 'org
  (setq org-directory
        (concat
         (getenv "HOME")
        "/nextcloud/documents/org/")))

(with-eval-after-load 'org
  (setq +org-capture-bookmarks-file
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/roam/20221004090130-bookmarks.org")))

(setq org-agenda-files
   '("/home/chu/nextcloud/documents/org/roam/20220726210346-important_dates.org"
     "/home/chu/nextcloud/documents/org/roam/20220822103211-engl_1030.org"
     "/home/chu/nextcloud/documents/org/roam/20220823133456-precalculus_algebra.org"
     "/home/chu/nextcloud/documents/org/roam/20220826102105-chem_1115.org"
     "/home/chu/nextcloud/documents/org/roam/20221002161631-my_conlang.org"
     "/home/chu/nextcloud/documents/org/roam/20221002190906-furry.org"
     "/home/chu/nextcloud/documents/org/roam/20221004221829-todo.org"
     "/home/chu/nextcloud/documents/org/roam/20221004221831-todo.org"
     "/home/chu/nextcloud/documents/org/roam/20221004222234-projects.org"
     "/home/chu/nextcloud/documents/org/roam/20221004222237-journal.org"
     "/home/chu/nextcloud/documents/org/roam/20221004222241-notes.org"
     "/home/chu/nextcloud/documents/org/roam/20240201170253-albums_to_download.org"
     "/home/chu/nextcloud/documents/org/roam/20240326161621-livestreaming.org"
     "/home/chu/nextcloud/documents/org/roam/asm/20240830094040-assembly.org"
     "/home/chu/nextcloud/documents/org/roam/c++/20240116111203-cpp.org"
     "/home/chu/nextcloud/documents/org/roam/daily/2024-05-08.org"
     "/home/chu/nextcloud/documents/org/roam/engl/engl-2020/20240116095712-engl_2020.org"
     "/home/chu/nextcloud/documents/org/roam/hist/hist-2320/20240116133242-hist_2320.org"
     "/home/chu/nextcloud/documents/org/roam/lisp/scheme/sicp/README.org"
     "/home/chu/nextcloud/documents/org/roam/math/20220821114043-mathematics.org"
     "/home/chu/nextcloud/documents/org/roam/math/20240903162832-linear_algebra.org"
     "/home/chu/nextcloud/documents/org/roam/math/20240905211621-calculus_ii.org"
     "/home/chu/nextcloud/documents/org/roam/20220726210347-important_dates.org"))

(with-eval-after-load 'org
  (setq +org-capture-journal-file
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/roam/20221004222230-journal.org")))

(with-eval-after-load 'org
  (setq org-journal-dir
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/roam/journal/")))

(with-eval-after-load 'org
  (setq +org-capture-notes-file
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/roam/20221004222235-notes.org")))

(with-eval-after-load 'org
  (setq +org-capture-projects-file
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/roam/20221004222226-projects.org")))

(with-eval-after-load 'org
  (setq +org-capture-todo-file
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/roam/20221004221829-todo.org")))

(with-eval-after-load 'org
  (setq org-roam-directory
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/roam/")))

(setq org-roam-dailies-directory "daily/")

(setq org-roam-dailies-capture-templates
      '(("d" "default" entry
         "* %?"
         :target (file+head "%<%Y-%m-%d>.org"
                            "#+title: %<%Y-%m-%d>\n"))))

(require 'org-roam-protocol)

(require 'org-roam-export)

(setq org-id-locations-file
      (concat
       (getenv "HOME")
       "/nextcloud/documents/org/.orgids"))

(setq org-attach-id-dir
      (concat
       (getenv "HOME")
       "/nextcloud/documents/org/.attach/"))

(setq org-cite-global-bibliography
       (list
        (concat
         (getenv "HOME")
         "/nextcloud/documents/org/roam/bib.bib")))

(setq org-cite-csl-styles-dir
      (concat
       (getenv "HOME")
       "/nextcloud/documents/org/latex/citeproc-formatters/"))

(setq org-archive-location "archives/%s_archive::")

(with-eval-after-load 'org
  (require 'org-download)
  ;; (add-hook 'dired-mode-hook 'org-download-enable) ; TODO See if we actually need this line.
  )

(after! org-mode
  (setq org-image-actual-width nil))

(with-eval-after-load 'ox-latex
  (add-to-list 'org-latex-classes
               '("org-plain-latex"
                 "\\documentclass{article}
           [NO-DEFAULT-PACKAGES]
           [PACKAGES]
           [EXTRA]"
                 ("\\section{%s}" . "\\section*{%s}")
                 ("\\subsection{%s}" . "\\subsection*{%s}")
                 ("\\subsubsection{%s}" . "\\subsubsection*{%s}")
                 ("\\paragraph{%s}" . "\\paragraph*{%s}")
                 ("\\subparagraph{%s}" . "\\subparagraph*{%s}"))))

(after! 'org
  (provide 'org-archive-subtree-hierarchical)
  (require 'org-archive)
  (defun org-archive-subtree-hierarchical--line-content-as-string ()
    "Returns the content of the current line as a string"
    (save-excursion
      (beginning-of-line)
      (buffer-substring-no-properties
       (line-beginning-position) (line-end-position))))
  (defun org-archive-subtree-hierarchical--org-child-list ()
    "This function returns all children of a heading as a list. "
    (interactive)
    (save-excursion
      ;; this only works with org-version > 8.0, since in previous
      ;; org-mode versions the function (org-outline-level) returns
      ;; gargabe when the point is not on a heading.
      (if (= (org-outline-level) 0)
          (outline-next-visible-heading 1)
        (org-goto-first-child))
      (let ((child-list (list (org-archive-subtree-hierarchical--line-content-as-string))))
        (while (org-goto-sibling)
          (setq child-list (cons (org-archive-subtree-hierarchical--line-content-as-string) child-list)))
        child-list)))
  (defun org-archive-subtree-hierarchical--org-struct-subtree ()
    "This function returns the tree structure in which a subtree belongs as a list."
    (interactive)
    (let ((archive-tree nil))
      (save-excursion
        (while (org-up-heading-safe)
          (let ((heading
                 (buffer-substring-no-properties
                  (line-beginning-position) (line-end-position))))
            (if (eq archive-tree nil)
                (setq archive-tree (list heading))
              (setq archive-tree (cons heading archive-tree))))))
      archive-tree))
  (defun org-archive-subtree-hierarchical ()
    "This function archives a subtree hierarchical"
    (interactive)
    (let ((org-tree (org-archive-subtree-hierarchical--org-struct-subtree))
          (this-buffer (current-buffer))
          (file (abbreviate-file-name
                 (or (buffer-file-name (buffer-base-buffer))
                     (error "No file associated to buffer")))))
      (save-excursion
        (setq location org-archive-location
              afile (car (org-archive--compute-location
                          (or (org-entry-get nil "ARCHIVE" 'inherit) location)))
              ;; heading (org-extract-archive-heading location)
              infile-p (equal file (abbreviate-file-name (or afile ""))))
        (unless afile
          (error "Invalid `org-archive-location'"))
        (if (> (length afile) 0)
            (setq newfile-p (not (file-exists-p afile))
                  visiting (find-buffer-visiting afile)
                  buffer (or visiting (find-file-noselect afile)))
          (setq buffer (current-buffer)))
        (unless buffer
          (error "Cannot access file \"%s\"" afile))
        (org-cut-subtree)
        (set-buffer buffer)
        (org-mode)
        (goto-char (point-min))
        (while (not (equal org-tree nil))
          (let ((child-list (org-archive-subtree-hierarchical--org-child-list)))
            (if (member (car org-tree) child-list)
                (progn
                  (search-forward (car org-tree) nil t)
                  (setq org-tree (cdr org-tree)))
              (progn
                (goto-char (point-max))
                (newline)
                (org-insert-struct org-tree)
                (setq org-tree nil)))))
        (newline)
        (org-yank)
        (when (not (eq this-buffer buffer))
          (save-buffer))
        (message "Subtree archived %s"
                 (concat "in file: " (abbreviate-file-name afile))))))
  (defun org-insert-struct (struct)
    "TODO"
    (interactive)
    (when struct
      (insert (car struct))
      (newline)
      (org-insert-struct (cdr struct))))
  (defun org-archive-subtree ()
    (org-archive-subtree-hierarchical)))

(after! 'org-archive
  (setq org-archive-default-command 'org-archive-subtree-hierarchical))

(setq org-capture-templates
  '(("t" "Personal todo" entry
    (file+headline +org-capture-todo-file "Inbox")
    "* [ ] %?\n%i\n%a" :prepend t)
    ("n" "Personal notes" entry
     (file+headline +org-capture-notes-file "Inbox")
     "* %u %?\n%i\n%a" :prepend t)
    ("j" "Journal" entry
     (file+olp+datetree +org-capture-journal-file)
     "* %U %?\n%i\n%a" :prepend t)
    ("p" "Templates for projects")
    ("pt" "Project-local todo" entry
     (file+headline +org-capture-project-todo-file "Inbox")
     "* TODO %?\n%i\n%a" :prepend t)
    ("pn" "Project-local notes" entry
     (file+headline +org-capture-project-notes-file "Inbox")
     "* %U %?\n%i\n%a" :prepend t)
    ("pc" "Project-local changelog" entry
     (file+headline +org-capture-project-changelog-file "Unreleased")
     "* %U %?\n%i\n%a" :prepend t)
    ("o" "Centralized templates for projects")
    ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
    ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
    ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)))

(after! org
  (use-package! vulpea
    :defer t
    :hook ((org-roam-db-autosync-mode . vulpea-db-autosync-enable))))

(defun org-babel-tangle-block ()
  (interactive)
  (let ((current-prefix-arg '(4)))
    (call-interactively 'org-babel-tangle)))

(after! org
  (map! :map org-mode-map
        :prefix "C-c C-v"
        "t" #'org-babel-tangle-block
        "T" #'org-babel-tangle))

(defun org-babel-tangle-from-edit-special ()
    (interactive)
    (org-edit-src-exit)
    (let ((current-prefix-arg '(4)))
      (call-interactively 'org-babel-tangle))
    (org-edit-special))

(after! org
  (map! :map org-src-mode-map
        "<f9>" #'org-babel-tangle-from-edit-special))

(defun markdown-convert-buffer-to-org ()
  "Convert the current buffer's content from markdown to orgmode format and save it with the current buffer's file name but with .org extension."
  (interactive)
  (shell-command-on-region (point-min) (point-max)
                           (format "pandoc -f markdown -t org -o %s"
                                   (concat (file-name-sans-extension (buffer-file-name)) ".org"))))

(setq erc-server "localhost"
      erc-nick "chuthepup"
      erc-user-full-name "Chu the Pup")

(require 'edit-server)
(edit-server-start)

(after! 'org
  (setq ispell-alternate-dictionary "/usr/share/dict"))

(setq auth-source-save-behavior nil)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

(after! dired
  (setq dired-backup-overwrite t))

(after! dired
  (setq delete-by-moving-to-trash t))

;; (defun dired-rsync-skip-newer (dest)
;;   "Asynchronously copy files in dired to `DEST' using rsync
;; set to resolve symlinks, skip files that are newer in `DEST',
;; and to run in archive mode."
;;   (interactive
;;    (list (read-file-name "rsync to: " (dired-dwim-target-directory)
;;                               nil nil nil 'file-directory-p)))
;;   (let ((dired-rsync-options "-aLuz --info=progress2"))
;;         (dired-rsync dest)))

;; (after! dired
;;   (map! :map dired-mode-map
;;         :prefix "C-c C-d"
;;         "C-r" #'dired-rsync-skip-newer))

;; (map! :map dirvish-mode-map "<normal-state> l" #'dired-find-file)
;; (map! :map dirvish-mode-map "<normal-state> h" #'dired-up-directory)
;; (map! :map dirvish-mode-map "<normal-state> q" #'dirvish-quit)
;; (map! :map general-override-mode-map "<normal-state> SPC o _" #'dirvish)

(after! dirvish
  (setq dirvish-reuse-session nil))

;; (use-package! abbrev-mode
;;   :defer t
;;   :hook (org-mode . abbrev-mode)
;;         (text-mode . abbrev-mode))

;; (after! 'projectile
;;   (when (and (not (executable-find "fd"))
;;              (executable-find "rg"))
;;     (setq projectile-generic-command
;;           (let ((rg-cmd ""))
;;             (dolist (dir projectile-globally-ignored-directories)
;;               (setq rg-cmd (format "%s --glob '!%s'" rg-cmd dir)))
;;             (setq rg-ignorefile
;;                   (concat "--ignore-file" " "
;;                           (expand-file-name "rg_ignore" user-emacs-directory)))
;;             (concat "rg -0 --files --color=never --hidden" rg-cmd " " rg-ignorefile))))
;;   (setq completion-styles '(orderless basic)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles basic partial-completion)))))

(after! emms
  (setq emms-source-file-directory-tree-function 'emms-source-file-directory-tree-find))

(after! emms
  (setq emms-source-file-default-directory "~/Music/"))

(use-package! elcord-mode
  :defer 360
  :config (elcord-mode))

(setq epg-pinentry-mode 'loopback)

(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\""
                                  (replace-regexp-in-string "%0A" "\n" desc)) prompt ": ")))) str))

(use-package tramp
  :defer t
  :config
  ;; Enable full-featured Dirvish over TRAMP on certain connections
  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1.
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:chunix:")
                     "direct-async-process" t))
  ;; Tips to speed up connections
  (setq tramp-verbose 0)
  (setq tramp-chunksize 2000)
  (setq tramp-use-ssh-controlmaster-options nil)
  ;; Ange-FTP defaults to =~/.netrc=  so you need to add this to your init script:
  (setq ange-ftp-netrc-filename "~/.authinfo.gpg"))

(use-package! palimpsest-mode
  :defer t
  :hook (prog-mode . palimpsest-mode))

(after! lisp-mode
  (defun +lisp/find-file-in-quicklisp ()
    "Find a file belonging to a library downloaded by Quicklisp."
    (interactive)
    (doom-project-find-file "~/.local/share/roswell/lisp/quicklisp/dists")))

(after! lisp-mode
  (load! (expand-file-name "~/.local/share/roswell/helper.el"))
  (setq inferior-lisp-program "ros dynamic-space-size=8000 -Q run"))

(after! lisp-mode
  (use-package! common-lisp-snippets
    :defer t))

(after! sly
  (setq sly-complete-symbol-function 'sly-flex-completions))

(after! cc-mode
  (setq c-default-style "doom"))

(use-package mu4e
  ;; :ensure nil
  ;; :load-path "/usr/share/emacs/site-lisp/mu4e/"
  ;; :defer 20 ; Wait until 20 seconds after startup
  :config
  ;; This is set to 't' to avoid mail syncing issues when using mbsync
  (setq mu4e-change-filenames-when-moving t)
  ;; Refresh mail using isync every 10 minutes
  (setq mu4e-update-interval (* 10 60))
  (setq mu4e-get-mail-command "mbsync -a")
  (setq mu4e-maildir "~/.local/share/mail/")
  (setq mu4e-drafts-folder "/[Gmail]/Drafts")
  (setq mu4e-sent-folder   "/[Gmail]/Sent Mail")
  (setq mu4e-refile-folder "/[Gmail]/All Mail")
  (setq mu4e-trash-folder  "/[Gmail]/Trash")
  (setq mu4e-maildir-shortcuts
        '((:maildir "/Inbox"    :key ?i)
          (:maildir "/[Gmail]/Sent Mail" :key ?s)
          (:maildir "/[Gmail]/Trash"     :key ?t)
          (:maildir "/[Gmail]/Drafts"    :key ?d)
          (:maildir "/[Gmail]/All Mail"  :key ?a)))
  (setq +mu4e-backend 'mbsync
        sendmail-program (executable-find "msmtp")
        send-mail-function #'smtpmail-send-it
        message-sendmail-f-is-evil t
        message-sendmail-extra-arguments '("--read-envelope-from")
        message-send-mail-function #'message-send-mail-with-sendmail
        mu4e-context-policy 'pick-first
        mu4e-contexts
        `(,(make-mu4e-context
            :name "Acalebdeanwatson@gmail.com"
            :enter-func (lambda () (mu4e-message "Entering calebdeanwatson@gmail.com context"))
            :leave-func (lambda () (mu4e-message "Leaving calebdeanwatson@gmail.com context"))
            :match-func (lambda (msg)
                          (when msg
                            (string-match-p "^/gmail.com/calebdeanwatson"
                                            (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "calebdeanwatson@gmail.com")
                    (user-full-name . "Caleb D. Watson")
                    (message-signature . (concat "Best regards,\n"
                                                 "Caleb D. Watson\n"))
                    (mu4e-sent-folder . "/gmail.com/calebdeanwatson/[Gmail]/Sent Mail")
                    (mu4e-drafts-folder . "/gmail.com/calebdeanwatson/[Gmail]/Drafts")
                    (mu4e-trash-folder . "/gmail.com/calebdeanwatson/[Gmail]/Trash")))
          ,(make-mu4e-context
            :name "Bchufilthymutt@gmail.com"
            :enter-func (lambda () (mu4e-message "Entering chufilthymutt@gmail.com context"))
            :leave-func (lambda () (mu4e-message "Leaving chufilthymutt@gmail.com context"))
            :match-func (lambda (msg)
                          (when msg
                            (string-match-p "^/gmail.com/chufilthymutt"
                                            (mu4e-message-field msg :maildir))))
            :vars '((user-mail-address . "chufilthymutt@gmail.com")
                    (user-full-name . "Chu")
                    (message-signature . nil)
                    (mu4e-sent-folder . "/gmail.com/chufilthymutt/[Gmail]/Sent Mail")
                    (mu4e-drafts-folder . "/gmail.com/chufilthymutt/[Gmail]/Drafts")
                    (mu4e-trash-folder . "/gmail.com/chufilthymutt/[Gmail]/Trash"))))))

(use-package whisper
  :defer t
  :config
  (setq whisper-install-directory "~/.config/emacs/.local/cache/"
        whisper-model "base"
        whisper-language "en"
        whisper-translate nil
        whisper-use-threads (/ (num-processors) 2))
        ;; turn off after 600 seconds of silence (regardless of input level)
        whisper-recording-timeout 600)

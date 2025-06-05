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
       "/.local/share/random-splash-images/"))

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

(defun chu/remove-all-empty-lines ()
  (interactive)
  (save-excursion
    (goto-char (point-min))
    (flush-lines "^\\s-*$")))

(setq org-tag-alist
      '(;; Places
        ("@home" . ?H)
        ("@work" . ?W)

        ;; Devices
        ("@computer" . ?C)
        ("@phone" . ?P)

        ;; Activities
        ("@planning" . ?n)
        ("@programming" . ?p)
        ("@writing" . ?w)
        ("@creative" . ?c)
        ("@email" . ?e)
        ("@calls" . ?a)
        ("@errands" . ?r)))

(setq org-log-into-drawer "LOGBOOK")

(add-hook 'org-mode-hook 'org-auto-tangle-mode)

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

;; (after! 'org
  (setq org-directory
        (concat
         (getenv "HOME")
        "/Nextcloud/documents/org/"))
  ;; )

(with-eval-after-load 'org
  (setq +org-capture-bookmarks-file
        (concat
         (getenv "HOME")
         "/Nextcloud/documents/org/roam/20221004090130-bookmarks.org")))

(setq org-agenda-files
   '("/home/chu/Nextcloud/documents/org/roam/20220726210346-important_dates.org"
     "/home/chu/Nextcloud/documents/org/roam/20220822103211-engl_1030.org"
     "/home/chu/Nextcloud/documents/org/roam/20220823133456-precalculus_algebra.org"
     "/home/chu/Nextcloud/documents/org/roam/20220826102105-chem_1115.org"
     "/home/chu/Nextcloud/documents/org/roam/20221002161631-my_conlang.org"
     "/home/chu/Nextcloud/documents/org/roam/20221002190906-furry.org"
     "/home/chu/Nextcloud/documents/org/roam/20221004221829-todo.org"
     "/home/chu/Nextcloud/documents/org/roam/20221004221831-todo.org"
     "/home/chu/Nextcloud/documents/org/roam/20221004222234-projects.org"
     "/home/chu/Nextcloud/documents/org/roam/20221004222237-journal.org"
     "/home/chu/Nextcloud/documents/org/roam/20221004222241-notes.org"
     "/home/chu/Nextcloud/documents/org/roam/20240201170253-albums_to_download.org"
     "/home/chu/Nextcloud/documents/org/roam/20240326161621-livestreaming.org"
     "/home/chu/Nextcloud/documents/org/roam/asm/20240830094040-assembly.org"
     "/home/chu/Nextcloud/documents/org/roam/c++/20240116111203-cpp.org"
     "/home/chu/Nextcloud/documents/org/roam/daily/2024-05-08.org"
     "/home/chu/Nextcloud/documents/org/roam/engl/engl-2020/20240116095712-engl_2020.org"
     "/home/chu/Nextcloud/documents/org/roam/hist/hist-2320/20240116133242-hist_2320.org"
     "/home/chu/Nextcloud/documents/org/roam/lisp/scheme/sicp/README.org"
     "/home/chu/Nextcloud/documents/org/roam/math/20220821114043-mathematics.org"
     "/home/chu/Nextcloud/documents/org/roam/math/20240903162832-linear_algebra.org"
     "/home/chu/Nextcloud/documents/org/roam/math/20240905211621-calculus_ii.org"
     "/home/chu/Nextcloud/documents/org/roam/20220726210347-important_dates.org"))

(with-eval-after-load 'org
  (setq +org-capture-journal-file
        (concat
         (getenv "HOME")
         "/Nextcloud/documents/org/roam/20221004222230-journal.org")))

(with-eval-after-load 'org
  (setq org-journal-dir
        (concat
         (getenv "HOME")
         "/Nextcloud/documents/org/roam/journal/")))

(with-eval-after-load 'org
  (setq +org-capture-notes-file
        (concat
         (getenv "HOME")
         "/Nextcloud/documents/org/roam/20221004222235-notes.org")))

(with-eval-after-load 'org
  (setq +org-capture-projects-file
        (concat
         (getenv "HOME")
         "/Nextcloud/documents/org/roam/20221004222226-projects.org")))

(with-eval-after-load 'org
  (setq +org-capture-todo-file
        (concat
         (getenv "HOME")
         "/Nextcloud/documents/org/roam/20221004221829-todo.org")))

;; (with-eval-after-load 'org
  (setq org-roam-directory
        (concat
         (getenv "HOME")
         "/Nextcloud/documents/org/roam/"))
;; )

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
       "/Nextcloud/documents/org/.orgids"))

(setq org-attach-id-dir
      (concat
       (getenv "HOME")
       "/Nextcloud/documents/org/.attach/"))

(setq org-cite-global-bibliography
       (list
        (concat
         (getenv "HOME")
         "/Nextcloud/documents/org/roam/bib.bib")))

(setq org-cite-csl-styles-dir
      (concat
       (getenv "HOME")
       "/Nextcloud/documents/org/latex/citeproc-formatters/"))

(setq org-archive-location "archives/%s_archive::")

(with-eval-after-load 'org
  (require 'org-download))

(setq org-image-actual-width nil)

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
  '(("t" "Personal todo" entry (file+headline +org-capture-todo-file "Inbox") "* [ ] %?\n%i\n" :prepend t)
    ("n" "Personal notes" entry (file+headline +org-capture-notes-file "Inbox") "* %u %?\n%i\n" :prepend t)
    ("j" "Journal" entry (file+olp+datetree +org-capture-journal-file) "* %U %?\n%i\n" :prepend t)
    ("p" "Templates for projects")
    ("pt" "Project-local todo" entry (file+headline +org-capture-project-todo-file "Inbox") "* TODO %?\n%i\n%a" :prepend t)
    ("pn" "Project-local notes" entry (file+headline +org-capture-project-notes-file "Inbox") "* %U %?\n%i\n%a" :prepend t)
    ("pc" "Project-local changelog" entry (file+headline +org-capture-project-changelog-file "Unreleased") "* %U %?\n%i\n%a" :prepend t)
    ("o" "Centralized templates for projects")
    ("ot" "Project todo" entry #'+org-capture-central-project-todo-file "* TODO %?\n %i\n %a" :heading "Tasks" :prepend nil)
    ("on" "Project notes" entry #'+org-capture-central-project-notes-file "* %U %?\n %i\n %a" :heading "Notes" :prepend t)
    ("oc" "Project changelog" entry #'+org-capture-central-project-changelog-file "* %U %?\n %i\n %a" :heading "Changelog" :prepend t)))

(define-key global-map (kbd "C-c j")
  (lambda () (interactive) (org-capture nil "j")))

(after! 'org
  (use-package! vulpea
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
  "Convert the current buffer's content from markdown to orgmode format
   and save it with the current buffer's file name but with .org extension."
  (interactive)
  (shell-command-on-region (point-min)
                           (point-max)
                           (format "pandoc -f markdown -t org -o %s"
                                   (concat (file-name-sans-extension
                                            (buffer-file-name)) ".org"))))

(add-to-list 'auto-mode-alist '("\\.\\(asm\\|s\\|S\\)$" . nasm-mode))

(defun ascii-table ()
  "Display basic ASCII table (0 thru 128)."
  (interactive)
  (switch-to-buffer "*ASCII*")
  (erase-buffer)
  (setq buffer-read-only nil)        ;; Not need to edit the content, just read mode (added)
  (local-set-key "q" 'bury-buffer)   ;; Nice to have the option to bury the buffer (added)
  (save-excursion (let ((i -1))
    (insert "ASCII characters 0 thru 127.\n\n")
    (insert " Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char|  Hex  Dec  Char\n")
    (while (< i 31)
      (insert (format "%4x %4d %4s | %4x %4d %4s | %4x %4d %4s | %4x %4d %4s\n"
                      (setq i (+ 1  i)) i (single-key-description i)
                      (setq i (+ 32 i)) i (single-key-description i)
                      (setq i (+ 32 i)) i (single-key-description i)
                      (setq i (+ 32 i)) i (single-key-description i)))
      (setq i (- i 96))))))

(setq erc-server "localhost"
      erc-nick "chuthepup"
      erc-user-full-name "Chu the Pup")

(require 'edit-server)
(edit-server-start)

(after! 'org
  (setq ispell-alternate-dictionary "/usr/share/dict"))

(setq auth-source-save-behavior nil)

(add-to-list 'auto-mode-alist '("\\.epub\\'" . nov-mode))

;; Ask to back files up prior to overwriting them
(setq dired-backup-overwrite t)

(setq delete-by-moving-to-trash t)

(setq dired-listing-switches "-hl -v --group-directories-first")

(after! dirvish
  ;; Hide dotfiles by default
  (setq dirvish-listing-switches "-hlv --group-directories-first")

  (defun dirvish-toggle-dotfiles ()
    "Toggle showing dotfiles in Dirvish."
    (interactive)
    (if (string-match-p "\\b-a\\b" dirvish-listing-switches)
        (setq dirvish-listing-switches "-hlv --group-directories-first") ; Hide dotfiles
      (setq dirvish-listing-switches "-ahlv --group-directories-first")) ; Show dotfiles
    (dirvish-refresh))

  (map! :map dirvish-mode-map
        "C-<escape>" #'dirvish-toggle-dotfiles))

(use-package! elcord-mode
  :defer t
  :config
  (setq elcord-display-buffer-details nil
        elcord-display-line-numbers nil
        elcord-quiet t
        elcord-mode t))

(setq emms-source-beets-database "/run/media/root/grandarchive/root/audio/music/library.db")

(setq epg-pinentry-mode 'loopback)

(defun pinentry-emacs (desc prompt ok error)
  (let ((str (read-passwd (concat (replace-regexp-in-string "%22" "\""
                                  (replace-regexp-in-string "%0A" "\n" desc)) prompt ": ")))) str))

(setq ange-ftp-netrc-filename "~/.authinfo.gpg")

(use-package tramp
  :config
  ;; Enable full-featured Dirvish over TRAMP on certain connections
  ;; https://www.gnu.org/software/tramp/#Improving-performance-of-asynchronous-remote-processes-1.
  (add-to-list 'tramp-connection-properties
               (list (regexp-quote "/ssh:chunix:")
                     "direct-async-process" t))
  ;; Tips to speed up connections
  (setq tramp-verbose 0
        tramp-chunksize 2000
        tramp-use-ssh-controlmaster-options nil))

(use-package! palimpsest-mode
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

;;(setq c-syntactic-indentation nil)

(use-package whisper
  :config
  (setq whisper-install-directory "~/.config/emacs/.local/cache/"
        whisper-model "base"
        whisper-language "en"
        whisper-translate nil
        whisper-use-threads (/ (num-processors) 2))
        ;; turn off after 600 seconds of silence
        whisper-recording-timeout 600)

(after! company
  (setq +lsp-company-backends '(company-tabnine :separate company-capf company-yasnippet))
  (setq company-show-numbers t)
  (setq company-idle-delay 0))

(defun newline-after-comma-in-parens ()
  "Insert a newline after each comma within the parentheses of the current line and re-indent."
  (interactive)
  (save-excursion
    (let ((start (line-beginning-position))
          (end (line-end-position)))
      (goto-char start)
      (while (re-search-forward ",\\s-*" end t)
        (replace-match ",\n" t t))
      (indent-region start (line-end-position)))))

(map! :n "SPC p ," #'newline-after-comma-in-parens) ;; Bind it to a key, like `SPC p ,`

(require 'ob-hledger) ;; depends on org-contrib package
(setq ledger-binary-path "hledger.sh"
      ledger-mode-should-check-version nil
      ledger-report-auto-width nil
      ledger-report-links-in-register nil
      ledger-report-native-highlighting-arguments '("--color=always")
      ledger-default-date-string "%Y-%m-%d"
      ledger-source-directory (getenv "LEDGER_FILE")
      ledger-init-file nil
      ;; ledger-init-file-name "~/.ledgerrc"
      ;; ledger-init-file-name "~/.config/ledger/ledgerrc"
      ledger-accounts-file nil
      ledger-schedule-file nil
      ledger-payees-file nil)

(add-to-list 'auto-mode-alist '("\\.hledger\\'" . ledger-mode))

;; (after! 'ledger-mode
;;   (setq ledger-report-use-strict t))

(defvar chu/default-currency "$"
  "Default currency symbol used for formatting amounts.")

(defun chu/ledger-format-number-with-commas (num)
  "Format NUM with commas and two decimals (e.g., 1,234.56)."
  (let* ((str (format "%.2f" num))
         (parts (split-string str "\\."))
         (int-part (car parts))
         (dec-part (cadr parts))
         (int-with-commas
          (replace-regexp-in-string
           "\\B\\(\\d\\{3\\}\\)\\(\\(?:\\d\\{3\\}\\)*\\)" ",\\1" int-part)))
    (concat int-with-commas "." dec-part)))

(defun chu/format-usd (amount)
  "Format AMOUNT as a USD value with commas, 2 decimals, and no space before $."
  (let* ((num (abs amount))
         (sign (if (< amount 0) "-" ""))
         (formatted (chu/ledger-format-number-with-commas num)))
    (format "%s%s%s" sign chu/default-currency formatted)))

(defun chu/ledger-fill-1-xact ()
  "Fill one missing posting in the current transaction with formatted amount."
  (pcase-let* ((`(,total . ,missing-positions) (ledger-post-xact-total))
               (missing-amount (ledger-negate-commodity total))
               (amounts-balance (< (abs (car missing-amount)) 0.0001)))
    (pcase missing-positions
      ('() (unless amounts-balance
             (user-error "Postings do not balance, but no posting to fill")))
      (`(,missing-pos)
       (if amounts-balance
           (user-error "Missing amount but amounts balance already")
         (goto-char missing-pos)
         (insert "  " (chu/format-usd (car missing-amount)))
         (ledger-post-align-xact (point))))
      (_ (user-error "More than one posting with missing amount")))))

(with-eval-after-load 'ledger-mode
  (defun chu/ledger-post-fill (&optional beg end)
    "Fill missing posting amounts.

If region is active, process all transactions in region.
Otherwise, process only the current transaction."
    (interactive (if (use-region-p)
                     (list (region-beginning) (region-end))))
    (save-excursion
      (if (and beg end)
          ;; Region-based processing
          (progn
            (goto-char beg)
            (while (and (< (point) end)
                        (re-search-forward ledger-transaction-start-regexp end t))
              (goto-char (match-beginning 0))
              (chu/ledger-fill-1-xact)
              (ledger-next-xact)))
        ;; Single transaction fallback
        (chu/ledger-fill-1-xact)))))

;; Optional: keybinding for Doom Emacs
(with-eval-after-load 'ledger-mode
  (map! :map ledger-mode-map
        :localleader
        "f" #'chu/ledger-post-fill))

(achievements-mode)

(parrot-mode)

;; "Set org-file-apps based on the system type"
;; (when IS-WSL
  (setq org-file-apps '((remote . emacs)
                        (auto-mode . emacs)
                        (directory . emacs)
                        ("\\.mm\\'" . "wslview \"%s\"")
                        ("\\.x?html?\\'" . "wslview \"%s\"")
                        ("\\.pptx?\\'" . "wslview \"%s\"")
                        ("\\.xlsx?\\'" . "wslview \"%s\"")
                        ("\\.docx?\\'" . "wslview \"%s\"")
                        ("\\.pdf\\'" .  "wslview \"%s\"")))
  ;; need to set explorer for open weblinks and htmls seperately
  (setq browse-url-generic-program "/mnt/c/Program Files/Mozilla Firefox/firefox.exe")
;; )

;; Windows 10 blocks Ctrl-Shift-0, So we using powertoy to cheat the system
;; when we press "C-)", it becomes "C->"
;; (cond (IS-WSL (map! "C->" #'sp-forward-slurp-sexp))
;;       (t (map! "C-)" #'sp-forward-slurp-sexp)))

(defun win2wsl-clipped-image()
  "use powershell to save the clipped image to wsl and load it to xclip"
  (let* ((powershell "/mnt/c/Windows/System32/WindowsPowerShell/v1.0/powershell.exe")
         (file-name "//wsl$/Arch/home/chu/tmp/clip_win2wsl.png")
         (file-name-wsl "~/tmp/clip_win2wsl.png")
         )
    (shell-command (concat powershell " -noprofile -command \"(Get-Clipboard -Format Image).Save(\\\"" file-name "\\\")\""))
    (call-process-shell-command (concat "xclip -selection clipboard -t image/png -i " file-name-wsl))
    )
  )
(advice-add 'org-download-clipboard :before #'win2wsl-clipped-image)

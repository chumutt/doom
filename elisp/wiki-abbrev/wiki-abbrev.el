;;; wiki-abbrev.el --- Abbrev wiki-misspelled words  -*- lexical-binding: t; -*-

;; Copyright (C) 2022  Case Duckworth  <acdw@acdw.net>
;; Keywords: convenience
;; Package-Version: 0.1
;; URL: https://codeberg.org/acdw/wiki-abbrev.el
;; Package-Requires: ((emacs "24.3"))

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <https://www.gnu.org/licenses/>.

;;; Commentary:

;; Based on [[https://www.masteringemacs.org/article/correcting-typos-misspellings-abbrev][Mickey Peterson's blog post on the matter]], except I want to do it
;; programmatically.  The list of commonly-misspelled words is pulled from [[https://en.wikipedia.org/wiki/Wikipedia:Lists_of_common_misspellings/For_machines][Wikipedia]].

;;; Code:

(require 'abbrev)
(require 'cl-lib)
(require 'url)

(defvar wiki-abbrev-misspellings-url
  "https://en.wikipedia.org/wiki/Wikipedia:Lists_of_common_misspellings/For_machines"
  "URL of common misspellings for processing.")

(defcustom wiki-abbrev-file (locate-user-emacs-file "wiki-abbrevs")
  "File to save common misspellings in.")

(defun wiki-abbrev-pairs (url &optional force)
  "Return pairs of corrections from URL.
Fetches the URL, then loops through the buffer's lines to build
an alist of (MISSPELLING . CORRECTON) pairs.

Abbreviations will be read from `wiki-abbrev-file', if it exists, or the
abbreviation list will be downloaded from URL, saved to that file, and parsed.

If FORCE is non-nil, the URL will be downloaded and saved regardless if it
already exists."
  (let ((pagebuf (if (or (not (file-exists-p wiki-abbrev-file))
                         force)
                     (url-retrieve-synchronously wiki-abbrev-misspellings-url)
                   (find-file-noselect wiki-abbrev-file)))
        abbrevs)
    (when pagebuf
      (with-current-buffer pagebuf
        ;; Remove all lines but the MISSPELLING->CORRECTION ones
        (keep-lines "[^-]->")
        ;; The first line has <pre>
        (replace-regexp "<pre>" "")
        ;; Remove lines with commas
        (flush-lines ",")
        ;; Loop, building the alist
        (setf abbrevs
              (cl-loop for line in (split-string (buffer-string) "\n")
                       as it = (split-string line "->")
                       collect (cons (car it) (cadr it)) into alist
                       finally return alist))
        (write-file wiki-abbrev-file))
      (kill-buffer pagebuf)
      abbrevs)))

(defun wiki-abbrev-define-abbrevs (&optional url abbrev-table force)
  "Integrate Wikipedia commonly-misspelled words into `abbrev'.
If URL is missing or nil, `wiki-abbrev-misspellings-url' will be used.

The URL should provide a plain-text mapping of misspellings to
corrections in this format: MISSPELLING->CORRECTION.  The list
can be embedded in other markup.

Optional argument ABBREV-TABLE is which abbreviation table to add
the misspellings to; if not given, it will be
`global-abbrev-table'.

FORCE is passed to `wiki-abbrev-pairs'."
  (message "Defining abbrevs...")
  (dolist (pair (wiki-abbrev-pairs (or url wiki-abbrev-misspellings-url) force))
    ;; Sanity checking
    (when (and (consp pair)
               (stringp (car pair))
               (stringp (cdr pair)))
      ;;(message "%s -> %s" (car pair) (cdr pair))
      (define-abbrev (or abbrev-table global-abbrev-table)
        (car pair)
        (cdr pair))))
  (message "Defining abbrevs...done."))

;;;###autoload
(defun wiki-abbrev-insinuate (&optional force)
  "Update Emacs `global-abbrev-table' from Wikipedia misspellings.
FORCE (interactively, \\[universal-argument]) will force a re-download."
  (interactive "P")
  (wiki-abbrev-define-abbrevs wiki-abbrev-misspellings-url global-abbrev-table force))

(provide 'wiki-abbrev)
;;; wiki-abbrev.el ends here

;;; find-file-in-quicklisp.el --- Description -*- lexical-binding: t; -*-
;; This file is not part of GNU Emacs.

(defun +lisp/find-file-in-quicklisp ()
  "Find a file belonging to a library downloaded by Quicklisp."
  (interactive)
  (doom-project-find-file "~/.local/share/roswell/lisp/quicklisp/dists"))

(provide '+lisp/find-file-in-quicklisp)
;;; find-file-in-quicklisp.el ends here

;;;; dynamic update of load-path is much better than this one.  But
;;;; ~/elisp/subdirs.el can not be used since when Emacs executable
;;;; all subdirs.el, directory ~/elisp is not in load-path yet.

;;; http://www.gnu.org/s/emacs/manual/html_node/elisp/Startup-Summary.html

;;; Step 1 running subdirs.el.
;;; Step 9 load user's init file.  Too late for ~/elisp/subdirs.el

(defconst wl-elisp-version "0.3")

(defun wl-elisp-version ()
  (interactive)
  (message "%s" wl-elisp-version))

(add-to-list 'load-path "~/elisp/3rd-party-lib")
(add-to-list 'load-path "~/elisp/customization")
(add-to-list 'load-path "~/elisp/lib")

(provide 'wl-load-path)

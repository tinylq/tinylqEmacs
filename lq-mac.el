(load "~/elisp/wl-load-path.el")
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp")
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/cogre")
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/common")
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/contrib")
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/ecb-images")
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/ede")
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/eieio")
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/info-help")
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/semantic")
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/speedbar")
(add-to-list 'load-path "/opt/local/share/emacs/site-lisp/srecode")

(add-to-list 'load-path "/Applications/lispbox-0.7/Emacs.app/Contents/MacOS")


(require 'wl-org)
(require 'lq-programming)
(require 'lq-generic)
(require 'of2org)

(load "lispbox")



(define-key function-key-map [S-tab] [backtab])
(add-hook 'term-setup-hook
          (lambda () (define-key input-decode-map "\e[Z" [backtab])))
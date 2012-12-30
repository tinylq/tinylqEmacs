(load "~/elisp/wl-load-path.el")
(setenv "PATH" (concat "c:/cygwin/bin;" (getenv "PATH")))
(setq exec-path (cons "c:/cygwin/bin/" exec-path))
(require 'cygwin-mount)
(cygwin-mount-activate)

(require 'wl-org)
(require 'lq-generic)
(require 'lq-programming)



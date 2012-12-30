(global-set-key "%" 'match-paren)

(global-set-key [f5] 'compile)
(set-default 'compile-command "scons ")
          
(defun match-paren (arg)
  "Go to the matching paren if on a paren; otherwise insert %."
  (interactive "p")
  (cond ((looking-at "\\s\(") (forward-list 1) (backward-char 1))
        ((looking-at "\\s\)") (forward-char 1) (backward-list 1))
        (t (self-insert-command (or arg 1)))))

(require 'xcscope) ;;加载xcscope\
(setq cscope-do-not-update-database t)
(setq cscope-database-regexps
      '(
        ( ".*"
          ( t )
          ("/usr/include/")
          )
        )
      )
		
(add-to-list 'load-path "~/elisp/3rd-party-lib/cedet")
(add-to-list 'load-path "~/elisp/3rd-party-lib/cedet/cogre")
(add-to-list 'load-path "~/elisp/3rd-party-lib/cedet/common")
(add-to-list 'load-path "~/elisp/3rd-party-lib/cedet/contrib")
(add-to-list 'load-path "~/elisp/3rd-party-lib/cedet/ede")
(add-to-list 'load-path "~/elisp/3rd-party-lib/cedet/eieio")

(add-to-list 'load-path "~/elisp/3rd-party-lib/cedet/semantic")
(add-to-list 'load-path "~/elisp/3rd-party-lib/cedet/speedbar")
(add-to-list 'load-path "~/elisp/3rd-party-lib/cedet/srecode")
(require 'cedet) ;;加载cedet
(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))
(global-set-key [f12] 'semantic-ia-fast-jump)


(add-to-list 'load-path "~/elisp/3rd-party-lib/ecb")
(add-to-list 'load-path "~/elisp/3rd-party-lib/ecb/info-help")
(require 'ecb) ;;加载ecb

(setq ecb-auto-activate nil
ecb-tip-of-the-day nil
ecb-tree-indent 4)


;;hippie-expand
(global-set-key (kbd "M-/") 'hippie-expand)

(setq hippie-expand-try-functions-list
      '(try-expand-all-abbrevs try-expand-dabbrev
        try-expand-dabbrev-all-buffers try-expand-dabbrev-from-kill
        try-complete-lisp-symbol-partially try-complete-lisp-symbol
        try-complete-file-name-partially try-complete-file-name))

;;auto-complete
(add-to-list 'load-path "~/elisp/3rd-party-lib/auto-complete")
(add-to-list 'load-path "~/elisp/3rd-party-lib/auto-complete/ac-dict")
(require 'auto-complete)
(require 'auto-complete-config)
(ac-config-default)

(add-hook 'c++-mode (lambda () (add-to-list 'ac-sources 'ac-source-semantic)))
;; Complete member name by C-c . for C++ mode.
;(add-hook 'c++-mode-hook
;          (lambda ()
;            (local-set-key (kbd "C-c .") 'ac-complete-semantic)))
;; Complete file name by C-c /
;(global-set-key (kbd "C-c /") 'ac-complete-filename)
(defun semantic-and-gtags-complete ()
  (interactive)
  (auto-complete '(ac-source-semantic ac-source-gtags)))

(defun auto-complete-settings ()
  "Settings for `auto-complete'."
  ;; After do this, isearch any string, M-: (match-data) always
  ;; return the list whose elements is integer
  (global-auto-complete-mode 1)
 
  ;; 不让回车的时候执行`ac-complete', 因为当你输入完一个
  ;; 单词的时候, 很有可能补全菜单还在, 这时候你要回车的话,
  ;; 必须要干掉补全菜单, 很麻烦, 用M-j来执行`ac-complete'

  ;(define-key ac-complete-mode-map "<return>"   'nil)
  ;(define-key ac-complete-mode-map "RET"        'nil)
  ;(define-key ac-complete-mode-map "M-j"        'ac-complete)
  ;(define-key ac-complete-mode-map "<C-return>" 'ac-complete)
  (define-key ac-complete-mode-map "\C-n"        'ac-next)
  (define-key ac-complete-mode-map "\C-p"        'ac-previous)
 
  (setq ac-dwim t)
  (setq ac-candidate-max ac-candidate-menu-height)
 
  (set-default 'ac-sources
               '(ac-source-semantic
                 ac-source-yasnippet
                 ac-source-abbrev
                 ac-source-words-in-buffer
                 ac-source-words-in-all-buffer
                 ac-source-imenu
                 ac-source-files-in-current-dir
                 ac-source-filename))
  ;(setq ac-modes ac+-modes)
 
  (dolist (command `(backward-delete-char-untabify delete-backward-char))
    (add-to-list 'ac-trigger-commands command))
 
  (defun ac-start-use-sources (sources)
    (interactive)
    (let ((ac-sources sources))
      (call-interactively 'ac-start)))
 
  (defvar ac-trigger-edit-commands
    `(self-insert-command
      delete-backward-char
      backward-delete-char
      backward-delete-char-untabify)
    "*Trigger edit commands that specify whether `auto-complete' should start or not when `ac-completing'."))
 
(eval-after-load "auto-complete"
  '(auto-complete-settings))
 
(eval-after-load "cc-mode"
  '(progn
     (dolist (command `(c-electric-backspace
                        c-electric-backspace-kill))
       (add-to-list 'ac-trigger-commands command)
       (add-to-list 'ac-trigger-edit-commands command))))
 
(eval-after-load "autopair"
  '(progn
     (dolist (command `(autopair-insert-or-skip-quote
                        autopair-backspace
                        autopair-extra-skip-close-maybe))
       (add-to-list 'ac-trigger-commands command))
 
     (defun ac-trigger-command-p ()
       "Return non-nil if `this-command' is a trigger command."
       (or
        (and
         (memq this-command ac-trigger-commands)
         (let* ((autopair-emulation-alist nil)
                (key (this-single-command-keys))
                (beyond-autopair (or (key-binding key)
                                     (key-binding (lookup-key local-function-key-map key)))))
           (memq beyond-autopair ac-trigger-edit-commands)))
        (and ac-completing
             (memq this-command ac-trigger-edit-commands))))))
 
(defun ac-settings-4-lisp ()
  "Auto complete settings for lisp mode."
  (setq ac-omni-completion-sources '(("\\<featurep\s+'" ac+-source-elisp-features)
                                     ("\\<require\s+'"  ac+-source-elisp-features)
                                     ("\\<load\s+\""    ac-source-emacs-lisp-features)))
  (ac+-apply-source-elisp-faces)
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-symbols
          ;; ac-source-semantic
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ;; ac-source-imenu
          ac-source-files-in-current-dir
          ac-source-filename)))
 
(defun ac-settings-4-java ()
  (setq ac-omni-completion-sources (list (cons "\\." '(ac-source-semantic))
                                         (cons "->" '(ac-source-semantic))))
  (setq ac-sources
        '(;;ac-source-semantic
          ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-files-in-current-dir
          ac-source-filename)))
 
(defun ac-settings-4-c ()
  (setq ac-omni-completion-sources (list (cons "\\." '(ac-source-semantic))
                                         (cons "->" '(ac-source-semantic))))
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-c-keywords
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-files-in-current-dir
          ac-source-filename)))
 
(defun ac-settings-4-cpp ()
  (setq ac-omni-completion-sources
        (list (cons "\\." '(ac-source-semantic))
              (cons "->" '(ac-source-semantic))))
  (setq ac-sources
        '(ac-source-yasnippet
          ac-source-c++-keywords
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-files-in-current-dir
          ac-source-filename)))
 
(defun ac-settings-4-text ()
  (setq ac-sources
        '(;;ac-source-semantic
          ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-imenu)))
 
(defun ac-settings-4-eshell ()
  (setq ac-sources
        '(;;ac-source-semantic
          ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-files-in-current-dir
          ac-source-filename
          ac-source-symbols
          ac-source-imenu)))
 
(defun ac-settings-4-ruby ()
  (require 'rcodetools-settings)
  (setq ac-omni-completion-sources
        (list (cons "\\." '(ac-source-rcodetools))
              (cons "::" '(ac-source-rcodetools)))))
 
(defun ac-settings-4-html ()
  (setq ac-sources
        '(;;ac-source-semantic
          ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-files-in-current-dir
          ac-source-filename)))
 
(defun ac-settings-4-tcl ()
  (setq ac-sources
        '(;;ac-source-semantic
          ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-files-in-current-dir
          ac-source-filename)))
 
(defun ac-settings-4-awk ()
  (setq ac-sources
        '(;;ac-source-semantic
          ac-source-yasnippet
          ac-source-abbrev
          ac-source-words-in-buffer
          ac-source-words-in-all-buffer
          ac-source-files-in-current-dir
          ac-source-filename)))

;;slime

;;括号匹配时显示另外一边的括号。
(show-paren-mode t)
(setq show-paren-style 'parentheses)

;;显示时间
(display-time)

;;按回车键后下一行代码自动缩进
(add-hook 'lisp-mode-hook '(lambda ()
      (local-set-key (kbd "RET") 'newline-and-indent))) 

(defvar ac-slime-modes
  '(lisp-mode))

(defun ac-slime-candidates ()
  "Complete candidates of the symbol at point."
  (if (memq major-mode ac-slime-modes)
      (let* ((end (point))
	     (beg (slime-symbol-start-pos))
	     (prefix (buffer-substring-no-properties beg end))
	     (result (slime-simple-completions prefix)))
	(destructuring-bind (completions partial) result
	  completions))))

(defvar ac-source-slime
  '((candidates . ac-slime-candidates)
    (requires-num . 3)))

(add-hook 'lisp-mode-hook (lambda ()
			    (slime-mode t)
			    (push 'ac-source-slime ac-sources)
			    (auto-complete-mode)))

(provide 'lq-programming)
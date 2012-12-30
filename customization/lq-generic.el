
(setq   default-tab-width   4) 
(setq-default indent-tabs-mode nil)
(setq tab-stop-list ())
(setq tab-width 4)
(set-fill-column 80)
(setq default-fill-column 100)

;;appt
(require 'appt)
(appt-activate 1)
(setq appt-issue-message t)
(setq appt-display-format 'window)

;;cursor
(set-cursor-color "black")
(blink-cursor-mode nil)

;;当前行高亮
(require 'hl-line)
(global-hl-line-mode 1)

;;自动删除
(setq
    backup-by-copying t ; 自动备份
    backup-directory-alist
    '(("." . "~/.saves")) ; 自动备份在目录"~/.saves"下
    delete-old-versions t ; 自动删除旧的备份文件
    kept-new-versions 6 ; 保留最近的6个备份文件
    kept-old-versions 2 ; 保留最早的2个备份文件
    version-control t) ; 多次备份
;; 显示行号
(require 'linum)
(setq linum-format "%3d ")
;对所有文件生效
(add-hook 'find-file-hooks (lambda () (linum-mode 1)))

;;中文
(setq current-language-environment "UTF-8")
(setq default-input-method "chinese-py-punct")

(set-language-environment 'Chinese-GB)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-buffer-file-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-selection-coding-system 'utf-8)
(modify-coding-system-alist 'process "*" 'utf-8)
(setq default-process-coding-system '(utf-8 . utf-8))
(setq-default pathname-coding-system 'utf-8)
(set-file-name-coding-system 'utf-8)
(set-clipboard-coding-system 'gb2312)
;发现这个时候写chinese-gb2312，或者是Chinese-GB都是可以的。
(setq x-select-enable-clipboard t)

;; calendar
(global-set-key [f11] 'calendar)

;;w3m
(add-to-list 'exec-path "/opt/local/bin/")
(setq w3m-use-favicon nil) 
(setq w3m-command-arguments '("-cookie" "-F")) 
(setq w3m-use-cookies t)
(setq w3m-home-page "http://www.baidu.com") 
;(setq w3m-default-display-inline-images t)

;;--------------窗口界面设置------------------
;;(set-foreground-color "green")
;;(set-background-color "black")

;; color theme
(add-to-list 'load-path "~/elisp/3rd-party-lib/color-theme")
(require 'color-theme)
(eval-after-load "color-theme"
  '(progn
     (color-theme-initialize)))
  
;;(desktop-read) 
;;(load "~/.emacs.framex") 

(require 'sr-speedbar)
(global-set-key (kbd "s-s") 'sr-speedbar-toggle)


;; (load "desktop-frame") 
;; ;;(add-hook 'desktop-save-hook
;; ;;           (lambda () 
;; ;;              (desktop-frame-save "~/"))) 
;; (defun my-desktop-frame-save ()
;;   (sr-speedbar-close)
;;   (desktop-frame-save "~/"))

;; (add-hook 'desktop-save-hook
;;           'my-desktop-frame-save)
                                       
;其中 ~/ 为存放.emacs.framex文件的目录，可任意指定 
;保证在desktop-save之前调用desktop-frame-save 
  
;;(desktop-read) 
;;(load "~/.emacs.framex") 

;; (require 'desktop)
;; (desktop-save-mode 1)
;; (defun my-desktop-save ()
;;   (interactive)
;;   ;; Don't call desktop-save-in-desktop-dir, as it prints a message.
;;   (if (eq (desktop-owner) (emacs-pid))
;;       (desktop-save desktop-dirname)))
;; (add-hook 'auto-save-hook 'my-desktop-save)

;; (desktop-save-mode 1) 

(provide 'lq-generic)
(add-to-list 'load-path "~/elisp/3rd-party-lib/org-mode/lisp")
(add-to-list 'load-path "~/elisp/3rd-party-lib/org-mode/contrib/lisp")

(require 'org)
(require 'org-clock)
(require 'org-id)			; C-c l creates CUSTOM_ID
;(require-maybe 'org-protocol)
;(require-maybe 'org-panel)
(add-to-list 'auto-mode-alist '("\\.org$" . org-mode))
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(add-hook 'org-mode-hook (lambda () (auto-fill-mode 1)))
(add-hook 'org-mode-hook 'turn-on-font-lock)
(add-hook 'org-mode-hook 
(lambda () (setq truncate-lines nil)))

(defun wl-turn-on-orgstruct-mode ()
  (orgstruct-mode 1))

(defun wl-turn-on-orgstruct++-mode ()
  (orgstruct++-mode 1))

(defun wl-turn-on-orgtbl-mode ()
  (orgtbl-mode 1))

(setq org-todo-keywords '((sequence "TODO(t)" "STARTED" "|" "DONE(d)")
			  (sequence "|" "CANCELED")
			  (sequence "WAITING" "|" "GOT")
			  (sequence "PROJECT" "|" "CLOSE")
			  (sequence "OPEN@BUG" "INVESTIGATE@BUG" "FIX@BUG"
				    "TEST@BUG" "REVIEW@BUG" "CLOSE@BUG" "|"
				    "FIXED@BUG" "TRANSFERRED@BUG")))

(setq org-todo-state-tags-triggers
      '(("STARTED" ("NEXT" . nil) ("someday" . nil))
	("INVESTIGATE@BUG" ("NEXT" . nil) ("someday" . nil))
	(done ("NEXT" . nil) ("someday" . nil))
	("WAITING" ("NEXT" . t))))

(setq org-todo-keyword-faces
      '(("WAITING" . (:foreground "gray" :weight bold))))

(setq org-stuck-projects
      '("+LEVEL=1|+LEVEL=2/+PROJECT" ("TODO") nil ""))

(add-to-list 'org-global-properties
	     '("Effort_ALL" . "0:10 0:30 1:00 2:00 3:00 4:00 5:00 6:00"))

(setq org-columns-default-format
      "%TODO %40ITEM %Effort{:} %CLOCKSUM{:} %TAGS")

(defun wl-org-column-view-uses-fixed-width-face ()
  ;; copy from org-faces.el
  (when (fboundp 'set-face-attribute)
    ;; Make sure that a fixed-width face is used when we have a column table.
    (set-face-attribute 'org-column nil
			:height (face-attribute 'default :height)
			:family (face-attribute 'default :family))))

(when (and (fboundp 'daemonp) (daemonp))
  (add-hook 'org-mode-hook 'wl-org-column-view-uses-fixed-width-face))

(setq org-agenda-include-diary t)

(defun wl-org-agenda-to-appt ()
  ;; Dangerous!!!  This might remove entries added by `appt-add' manually. 
  (org-agenda-to-appt t "TODO"))

(wl-org-agenda-to-appt)
(add-hook 'org-finalize-agenda-hook 'wl-org-agenda-to-appt)
(add-hook 'org-finalize-agenda-hook 'delete-other-windows)

;;; %.30s make `format' fail on some subject in Chinese
(setq org-email-link-description-format "Email %c: %s")
(setq org-icalendar-include-todo t)
(setq org-insert-mode-line-in-empty-file t)
(setq org-log-done 'note)
(setq org-reverse-note-order t)
(setq org-clock-persist t)
(org-clock-persistence-insinuate)
(setq org-clock-history-length 15)
(setq org-completion-use-iswitchb t)
(setq org-clock-modeline-total 'current)
(setq org-agenda-clockreport-parameter-plist '(:link t :maxlevel 3))
(setq org-deadline-warning-days 14)	; do not worry too much

;;; modify from `org-agenda-clock-goto'
(defun wl-org-agenda-clock-goto ()
  (interactive)
  (let (pos)
    (mapc (lambda (o)
	    (if (eq (overlay-get o 'type) 'org-agenda-clocking)
		(setq pos (overlay-start o))))
	  (overlays-in (point-min) (point-max)))
    (cond (pos (goto-char pos))
	  (org-clock-current-task
	   (org-clock-goto))
	  (t (error "No running clock")))))

(defun wl-org-add-note-or-done (&optional arg)
  "Add note to current clocked task from anywhere.  With ARG,
close it.  Or current task if not clocking"
  (interactive "p")
  ;; Go to clocking task if any
  (when (org-clocking-p)
    (if (eq major-mode 'org-agenda-mode)
	;; FIXME: Cursor doesn't move if clocking task is not in
	;; current agenda buffer.  So current task is closed
	;; incorrectly.
	(wl-org-agenda-clock-goto)
      (org-clock-goto)))
  ;; add note or mark it as done.
  (if (= 4 arg)
      (if (eq major-mode 'org-agenda-mode)
	  (org-agenda-todo 'done)
	(org-todo 'done))
    (if (eq major-mode 'org-agenda-mode)
	(org-agenda-add-note)
      (org-add-note))))

(global-set-key (kbd "<f6>") 'wl-org-add-note-or-done)

(defun wl-customize-org-agenda-custom-commands ()
  (interactive)
  (unless (file-exists-p "~/gtd/")
    (error "gtd files not found."))

  (setq org-agenda-custom-commands
	'(("g" "Calendar"
	   agenda "" ((org-agenda-ndays 1)
		      (org-deadline-warning-days 7)))
	  ("p" "Pomodoro"
	   ((tags-todo "+work-someday/STARTED|OPEN@BUG|INVESTIGATE@BUG|FIX@BUG|TEST@BUG|REVIEW@BUG|CLOSE@BUG"
		       ((org-agenda-overriding-header "Work tasks")))
	    (tags-todo "-work/STARTED"
		       ((org-agenda-overriding-header "Non-work tasks")))
	    (tags-todo "pomodoro")))
	  ("o" "Agenda and customized information"
	   ((tags-todo "top")
	    (tags-todo "-someday/PROJECT")
	    (tags-todo "+someday/PROJECT")
	    (agenda "" ((org-agenda-ndays 1)))
	    (tags-todo "NEXT")
	    (tags-todo "someday")))
	  ("Q" . "Custom queries")
	  ("Qm" "Search tags in todo and note"
	   (lambda (match) (org-tags-view nil)) ""
	   ((org-agenda-files (wl-expand-org-file-list '("todo.org" "notes.org")))))
	  ("QM" "Search tags in todo, note, and archives"
	   (lambda (match) (org-tags-view nil)) ""
	   ((org-agenda-files (file-expand-wildcards
			       (expand-file-name "*.org" org-directory)))))
	  ("Qs" "Search in todo and note"
	   (lambda (match) (org-search-view nil)) ""
	   ((org-agenda-files (wl-expand-org-file-list '("todo.org" "notes.org")))))
	  ("QS" "Search tags in todo, note, and archives"
	   (lambda (match) (org-search-view nil)) ""
	   ((org-agenda-files (file-expand-wildcards
			       (expand-file-name "*.org" org-directory)))))

	  ("w" . "Work-only related tasks")
	  ("wg" "Calendar"
	   agenda "" ((org-agenda-ndays 1))
	   ((org-agenda-files (wl-expand-org-file-list '("work.org")))))
	  ("wp" "Pomodoro"
	   ((tags-todo "quarterly_plan/TODO")
	    (tags-todo "-someday/STARTED|OPEN@BUG|INVESTIGATE@BUG|FIX@BUG|TEST@BUG|REVIEW@BUG|CLOSE@BUG")
	    (tags-todo "pomodoro"))
	   ((org-agenda-files (wl-expand-org-file-list '("work.org")))))
	  ("wo" "Agenda and customized information"
	   ((agenda "" ((org-agenda-ndays 1)))
	    (tags-todo "top")
	    (tags-todo "+quarterly_plan/PROJECT")
	    (tags-todo "-quarterly_plan/PROJECT")
	    (tags-todo "work+quarterly_plan/TODO")
	    (tags-todo "NEXT+gcc")
	    (tags-todo "NEXT+msa")
	    (tags-todo "NEXT-v8-gcc-msa")
	    (tags-todo "NEXT+v8|v8-someday/TODO")
	    (tags-todo "someday"))
	   ((org-agenda-files (wl-expand-org-file-list '("work.org")))))
	  ("wm" "Search tags"
	   (lambda (match) (org-tags-view nil)) ""
	   ((org-agenda-files (wl-expand-org-file-list '("work.org")))))
	  ("wM" "Search tags, archives included"
	   (lambda (match) (org-tags-view nil)) ""
	   ((org-agenda-files (wl-expand-org-file-list
			       '("work.org" "archive-2009-work.org")))))
	  ("ws" "Search"
	   (lambda (match) (org-search-view nil)) ""
	   ((org-agenda-files (wl-expand-org-file-list '("work.org")))))
	  ("wS" "Search, archives included"
	   (lambda (match) (org-search-view nil)) ""
	   ((org-agenda-files (wl-expand-org-file-list
			       '("work.org" "archive-2009-work.org"))))))))

(when (file-exists-p "~/gtd/")
  (setq org-directory "~/gtd/")

  ;; helper functions
  (defun wl-expand-org-file (file &optional directory)
    (let ((dir (or directory org-directory)))
      (expand-file-name file dir)))

  (defun wl-expand-org-file-list (file-list &optional directory)
    (mapcar '(lambda (file) (wl-expand-org-file file directory))
	    file-list))

  ;; org files
  (defconst wl-org-son-file (wl-expand-org-file "son.org"))
  (defconst wl-org-gtd-file (wl-expand-org-file "gtd.org"))
  (defconst wl-org-work-file (wl-expand-org-file "work.org"))
  (defconst wl-org-todo-file (wl-expand-org-file "todo.org"))
  (defconst wl-org-notes-file (wl-expand-org-file "notes.org"))
  (defconst wl-org-misc-file (wl-expand-org-file "misc.org"))
  (defconst wl-org-someday-file (wl-expand-org-file "someday.org"))
  (defconst wl-org-journal-file (wl-expand-org-file "journal.org"))


  (wl-customize-org-agenda-custom-commands)

  
;; org-capture
  (define-key global-map "\C-cc" 'org-capture)
  (setq org-capture-templates
	`(("e" "Email" entry (file+headline ,wl-org-work-file "Inbox")
	   "* TODO %?\n  :PROPERTIES:\n  :LOGGING: nil\n  :END:\nCreated on %u\n\n  %a" :prepend t)

	  ("w" "Work" entry (file+headline ,wl-org-work-file "Inbox")
	   "* TODO %?\nCreated on %u\n\n  %i\n" :prepend t)

	  ("l" "Link" entry (file+headline ,wl-org-todo-file "Tasks")
	   "* TODO %?\nCreated on %u\n\n  %a" :prepend t)

	  ("p" "Creating project")
	  ("pt" "Project for todo" entry (file+headline ,wl-org-todo-file "Tasks")
	   "* PROJECT %?\nCreated on %u\n\n  %i" :prepend t)
	  ("pw" "Project for work" entry (file+headline ,wl-org-work-file "Inbox")
	   "* PROJECT %?\nCreated on %u\n\n  %i" :prepend t)

	  ("t" "Todo" entry (file+headline ,wl-org-todo-file "Tasks")
	   "* TODO %?\nCreated on %u\n\n  %i" :prepend t)

	  ("n" "Note" entry (file+headline ,wl-org-notes-file "Notes")
	   "* %? \n%U \n\n  %i" :prepend t :empty-lines 1)

	  ("r" "Protocol" entry (file+headline ,wl-org-notes-file "Notes")
	   "* %U\n\n  %c\n\n  %i" :prepend t :empty-lines 1)
      
      ("j" "Journal" entry (file+datetree ,wl-org-journal-file)
       "* %?\nEntered on %U\n %i\n %a ")

	  ("a" "Account" table-line
	   (file+headline "~/edata/liang.org.gpg" "Web accounts")
	   "| %? | | %a | %U |")))

(dolist (todo (list wl-org-gtd-file wl-org-son-file wl-org-work-file wl-org-todo-file wl-org-misc-file wl-org-someday-file wl-org-notes-file))
    (when (file-exists-p todo)
      (add-to-list 'org-agenda-files todo))))

(setq-default org-tag-persistent-alist
	      '(("work") ("life") ("top")
		("tech" . ?t) ("finance" . ?f) ("gtd" . ?g) ("edu" . ?e)
		("writing" . ?w) ("coding" . ?c) ("thinking" . ?i)
		("reading" . ?r) ("leading") ("planning" . ?p) ("shopping")
		("tip")
		(:startgroup . nil)
		("NEXT" . ?n) ("someday" . ?s)
		(:endgroup . nil)
		(:startgroup . nil)
		("@office" . ?o) ("@home" . ?h)
		(:endgroup . nil)))

  ;; refile
  (setq tmp-files (file-expand-wildcards "~/gtd/*.org"))
  (setq org-refile-targets '((tmp-files . (:maxlevel . 1))))
  (setq org-agenda-text-search-extra-files (file-expand-wildcards "~/gtd/*.org"))
;  (setq org-refile-use-outline-path t)

;;; task switch

(defun wl-org-agenda-clock-in-prev-task ()
  (org-agenda-clock-goto)
  (org-clock-in '(4))
  (let ((current-prefix-arg nil))
    (org-agenda-redo)))

(defun wl-org-clock-in-previous-task ()
  "Clock in previous clocked task."
  (interactive)
  (if (and (org-clocking-p)
	   (eq major-mode 'org-agenda-mode))
      (wl-org-agenda-clock-in-prev-task)
    ;; Both `save-excursion' and `save-current-buffer' fail to restore
    ;; buffer.  So use this poor man solution.
    (let ((cb (current-buffer)))
      (org-clock-goto) ; Probably by this (call to `switch-to-buffer')?
      (org-clock-in (if (org-clocking-p) '(4) nil))
      (switch-to-buffer cb)))
  (org-save-all-org-buffers))

(defun wl-org-task-dispatch (&optional arg)
  (interactive "P")
  (if (equal arg '(4))
      (wl-org-clock-in-previous-task)
    (org-capture)))

(define-key global-map [(f8)] 'wl-org-task-dispatch)

;;; export

(unless (fboundp 'org-export-as-latex)
  ;; exists in old version of org
  (require 'org-export-latex))

(defun wl-org-export-as-latex ()
  (interactive)
  (let ((org-export-latex-special-keyword-regexp
	 (concat "^[ \t]*" org-export-latex-special-keyword-regexp)))
    (org-export-as-latex 3 nil nil nil t nil)))

(defun wl-org-export-region-as-html-string (beg end)
  (interactive "r")
  (save-excursion
    (org-export-region-as-html beg end t 'string)))

(defun wl-org-export-table-as-html ()
  (interactive)
  (unless (org-at-table-p)
    (error "No table at point"))
  (let* ((beg (org-table-begin))
	 (end (org-table-end))
	 (buffer
	  (org-export-region-as-html beg end t "*Org Table HTML Export*")))
    (switch-to-buffer-other-window buffer)))

(eval-after-load 'yasnippet
  '(yas/define-snippets
   'org-mode
   '(("blk" "#+BEGIN_$1 $2
  $0
#+END_$1" "#+BEGIN_ ... #+END_"))))

;;; publish

;; (require 'org-publish)                  
;; (defvar wl-org-publish-project-root-directory "~/project/wanglianghome.org/orgpublish/")
;; (defvar lq-org-publish-project-root-directory "~/")
;; (setq org-publish-project-alist
;;       `(("org"
;; 	 :base-directory ,(expand-file-name "src/" wl-org-publish-project-root-directory)
;; 	 :publishing-directory ,(expand-file-name "pub/" wl-org-publish-project-root-directory)
;; 	 :recursive t
;; 	 :section-numbers nil
;; 	 :table-of-contents nil)
;; 	("emacsbook"
;; 	 :base-directory ,(expand-file-name "emacsbook/" wl-org-publish-project-root-directory)
;; 	 :base-extension "org"
;; 	 :publishing-directory ,(expand-file-name "pub/programming/emacsbook/" wl-org-publish-project-root-directory))
;; 	("wl-org"
;; 	 :components ("org" "emacsbook"))
;;      ("luqi-org"
;; 	 :base-directory ,(expand-file-name "gtd/" lq-org-publish-project-root-directory)
;; 	 :base-extension "org"
;; 	 :publishing-directory ,(expand-file-name "pub/" lq-org-publish-project-root-directory))      
;;      ))

(require 'org-publish)


(setq note-root-dir "~/gtd")

(setq note-publish-dir "~/gtd/publish")


(setq org-publish-project-alist
      `(("note-org"
         :base-directory ,note-root-dir
         :publishing-directory ,note-publish-dir
         :base-extension "org"
         :recursive t
         :publishing-function org-publish-org-to-html
         :auto-index t
         :makeindex t
         :index-filename "index.org"
         :index-title "index"
         :link-home "index.html")
        ("note-static"
         :base-directory ,note-root-dir
         :publishing-directory ,note-publish-dir
         :recursive t
         :base-extension "css//|js//|png//|jpg//|gif//|pdf//|mp3//|swf//|zip//|gz//|txt//|el"
         :publishing-function org-publish-attachment)
        ("note" :components ("note-org" "note-static"))))


;;; statistics

(defun wl-org-entry-get-clocking ()
  (interactive)
  (remove nil
	  (mapcar (lambda (item)
		    (if (equal "CLOCK" (car item))
			(cdr item)
		      nil))
		  (org-entry-properties))))

(defun wl-org-entry-get-clocking-date ()
  (interactive)
  (remove ""
	  (mapcar (lambda (item)
		    (if (string-match "\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\).*\\([0-9]\\{4\\}-[0-9]\\{2\\}-[0-9]\\{2\\}\\)" item)
			(let ((beg (match-string 1 item))
			      (end (match-string 2 item)))
			  (if (string-equal beg end)
			      beg
			    end))
		      ""))
		  (wl-org-entry-get-clocking))))

(defun wl-org-entry-get-clocking-time (date)
  (interactive)
  (remove ""
	  (mapcar (lambda (item)
		    (if (string-match (concat date ".*=>  \\([0-9]+:[0-9]+\\)") item)
			(match-string 1 item)
		      ""))
		  (wl-org-entry-get-clocking))))

(defun wl-split-time-hour-minute (time)
  (if (string-match "\\([0-9]+\\):\\([0-9]+\\)" time)
      (cons (match-string 1 time) (match-string 2 time))
    nil))

(defun wl-to-minutes (time)
  (if (stringp time)
      (let ((l (wl-split-time-hour-minute time)))
	(+ (string-to-int (car l))
	   (string-to-int (cdr l))))
    time))

(defun wl-org-entry-get-clocking-time-sum (date)
  (interactive)
  (reduce (lambda (a b)
	    (+ (wl-to-minutes a) (wl-to-minutes b)))
	  (wl-org-entry-get-clocking-time date)))

(defun wl-org-show-current-entry-statistics ()
  (interactive)
  (let (stat)
    (dolist (date (wl-org-entry-get-clocking-date) stat)
      (add-to-list 'stat (cons date (wl-org-entry-get-clocking-time-sum date))))
    (when stat
      (wl-org-show-statistics (list (cons (org-get-heading) stat))))))

(defun wl-org-show-statistics-table (table)
  (insert "| DATE | CLOCKSUM |\n")
  (insert "|---\n")
  (mapcar (lambda (item)
	    (insert (concat "| "
			    (car item)
			    " | "
			    (int-to-string (wl-to-minutes (cdr item)))
			    " |\n")))
	  table)
  (insert "|---\n")
  (forward-line -1)
  (org-table-align)
  (forward-line 1))

(defun wl-org-show-statistics (output)
  (with-current-buffer (get-buffer-create "*Org Statistics*")
    (erase-buffer)
    (org-mode)
    (mapc (lambda (table)
	    (insert (concat "* " (car table) "\n\n"))
	    (wl-org-show-statistics-table (cdr table))
	    (insert "\n"))
	  output))
  (switch-to-buffer-other-window "*Org Statistics*"))

(defun wl-org-show-current-subtree-statistics ()
  (interactive)
  (let (output)
    (org-map-entries
     (lambda ()
       (let (stat)
	 (dolist (date (wl-org-entry-get-clocking-date) stat)
	   (when date
	     (add-to-list 'stat (cons date (wl-org-entry-get-clocking-time-sum date)))))
	 (when stat
	   (add-to-list 'output (cons (org-get-heading) stat)))))
     nil 'tree)
    (wl-org-show-statistics output)))

(setq org-src-fontify-natively t) 

;; journal

(org-remember-insinuate)
(setq org-default-notes-file (concat org-directory (concat "/journal/"  (concat (format-time-string "%Y-%m-%d") ".org"))))

;; mobileOrg

(setq org-mobile-directory "~/Dropbox/MobileOrg/")
(setq org-mobile-inbox-for-pull "~/gtd/inbox.org")

(provide 'wl-org)

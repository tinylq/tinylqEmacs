;;; of2org.el --- convert OmniFocus CSV exported files to org-mode files

;; Copyright (C) 2010  Magnus Henoch

;; Author: Magnus Henoch <magnus.henoch@gmail.com>
;; URL: http://bitbucket.org/legoscia/of2org

;; This program is free software; you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;;; Commentary:

;; This is a small program to convert files exported from OmniFocus
;; (in CSV format) into org-mode files.

;; To export from OmniFocus, go into Project view, select both "Inbox"
;; and "Library" (or just the projects you want, for a selective
;; export), change the Action filter from "Remaining" to "Any status"
;; (if you want to keep completed tasks), and select Export from the
;; File menu.  Select one of the CSV formats for exporting.

;; To import to org-mode, evaluate this file somehow (M-x eval-buffer,
;; M-x load-file, or whatever you feel like) and type M-x of2org, and
;; give the file to import and the file to export it to.

;; New versions and an issue tracker can be found at
;; <http://bitbucket.org/legoscia/of2org>.

;; Requires Ulf Jasper's csv.el from
;; <http://ulf.epplejasper.de/EmacsGeneralPurpose.html>.

;;; Code:
(require 'cl)
(require 'csv)

(defun of2org (of-file org-file)
  (interactive "fOmniFocus CSV file to import: 
FOrg mode file to create: ")
  (let ((of-data
	 (with-temp-buffer
	   (insert-file-contents of-file)
	   (goto-char (point-min))
	   ;; We may get lots of ^M characters because of how
	   ;; OmniFocus exports files: each record is separated by
	   ;; CRLF, but newlines inside a record are (usually?) just
	   ;; LF.  Emacs thus keeps all CR characters in order not to
	   ;; lose information, but we know it's safe to remove them.
	   (while (search-forward "\r" nil t)
	     (delete-char -1))
	   (csv-parse-buffer t))))
    ;; So now we have an alist with keys "Task ID", "Type" (only
    ;; "Inbox", "Action" or "Project", it seems), "Task", "Project",
    ;; "Context", "Start Date", "Due Date", "Completion Date",
    ;; "Duration", "Flagged" and "Notes".
    (with-temp-file org-file
      (org-mode)
      (dolist (entry of-data)
	(let (
	      ;; Task ID contains one dot per level, minus one.
	      (level (1+ (count ?. (of2org-get "Task ID" entry))))
	      (type (of2org-get "Type" entry))
	      (task (or (of2org-get "Task" entry) "unnamed task"))
	      ;; We don't need Project, as it's evident from the hierarchy.
	      (context (of2org-get "Context" entry))
	      (start-date (of2org-get "Start Date" entry))
	      (due-date (of2org-get "Due Date" entry))
	      (completion-date (of2org-get "Completion Date" entry))
	      (duration (of2org-get "Duration" entry))
	      (flagged (of2org-get "Flagged" entry))
	      (notes (of2org-get "Notes" entry)))
	  (when (string= type "Inbox")
	    ;; Inbox doesn't have a name; let's give it one.
	    (setq task "Inbox"))
	  (let ((todo-state
		 (cond
		  ;; Inbox or Project entries are never TODO
		  ((member type '("Inbox" "Project"))
		   " ")
		  ;; If it has a completion date, it's DONE
		  (completion-date
		   " DONE ")
		  ;; Otherwise, it's TODO
		  (t
		   " TODO "))))
	    (insert (make-string level ?*) todo-state task "\n"))
	  (org-entry-put (point) "Type" type)
	  (when context
	    (org-toggle-tag context 'on)
	    (org-entry-put (point) "Context" context))
	  (when start-date (org-schedule nil (date-to-time start-date)))
	  (when due-date (org-deadline nil (date-to-time due-date)))
	  (when completion-date (org-add-planning-info 'closed (date-to-time completion-date)))
	  (when duration
	    ;; It seems that OmniFocus always stores effort as 42m.
	    ;; `string-to-number' ignores trailing non-digits, so it's
	    ;; perfect.
	    (let ((minutes (string-to-number duration)))
	      (org-entry-put (point) org-effort-property
			     (format "%d:%02d" (/ minutes 60) (% minutes 60)))))
	  (when (string= flagged "1")
	    (org-toggle-tag "flagged" 'on))
	  (when notes (insert notes "\n")))))))

(defun of2org-get (key alist)
  "Get the value for KEY in ALIST, unless it is the empty string.
Return nil if KEY is not present in ALIST, or if the value for
KEY is the empty string."
  (let ((value (cdr (assoc key alist))))
    (and (not (zerop (length value))) value)))

(provide 'of2org)
;;; of2org.el ends here
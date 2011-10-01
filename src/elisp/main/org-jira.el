;; org-jira.el -- Provide an interface between Atlassian Jira and Org-Mode

;; Copyright (C) 2011  Michael Gregson <mgregson@csclub.uwaterloo.ca>

;; This program is free software: you can redistribute it and/or modify
;; it under the terms of the GNU General Public License as published by
;; the Free Software Foundation, either version 3 of the License, or
;; (at your option) any later version.

;; This program is distributed in the hope that it will be useful,
;; but WITHOUT ANY WARRANTY; without even the implied warranty of
;; MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;; GNU General Public License for more details.

;; You should have received a copy of the GNU General Public License
;; along with this program.  If not, see <http://www.gnu.org/licenses/>.

;; Author: Michael Gregson (mgregson@csclub.uwaterloo.ca)
;; Created: September, 2011
;; Keywords: jira, org-mode
;; Homepage: http://github.com/mgregson/org-jira

;; Uses emacs-soap-client and the sample jira2.el application to speak to Jira
;; See http://code.google.com/p/emacs-soap-client

;; Jira References:
;;
;; http://confluence.atlassian.com/display/JIRA/Creating+a+SOAP+Client
;;
;; JavaDoc for the Jira SOAP service
;; http://docs.atlassian.com/software/jira/docs/api/rpc-jira-plugin/latest/com/atlassian/jira/rpc/soap/JiraSoapService.html

(require 'soap-client)
(require 'org-compat)

(defgroup org-jira nil
	"Jira/Org-Mode Integration"
	:group 'tools)

(defcustom org-jira-host-url
	"http://jira.atlassian.com"
	"The base URL for your Jira service."
	:type 'string
	:group 'org-jira)

(defcustom org-jira-issues-limit
	30
	"Org-Jira will request at most this org-jira-issues-limit issues from Jira at
a time."
	:type 'int
	:group 'org-jira)

(defcustom org-jira-outgoing-buffer-file
	"~/org/jira/outgoing.org"
	"Org-Jira buffers outgoing Jira issues for later dispatch."
	:type 'string
	:group 'org-jira)

(defcustom org-jira-username
	nil
	"Org-Jira username for authentication."
	:type 'string
	:group 'org-jira)

(defcustom org-jira-issue-keyword
	"JIRA"
	"Org-Jira Issue Keyword.

Issues will look like:
* (org-jira-issue-keyword) (summary)..."
	:type 'string
	:group 'org-jira)

(defcustom org-jira-comment-keyword
	"COMMENT"
	"Org-Jira Comment Keyword

Comments will look like:
* (org-jira-comment-keyword) (author) spake at (time)..."
	:type 'string
	:group 'org-jira)

(defcustom org-jira-prop-prefix
	"JIRA"
	"Org-Jira will prefix all Jira provided properties with the
org-jira-prop-prefix."
	:type 'string
	:group 'org-jira)

(defvar org-jira-wsdl nil)
(defvar org-jira-filters nil)
(defvar org-jira-resolutions nil)
(defvar org-jira-auth-token nil)
(defvar org-jira-service "jirasoapservice-v2")

(defvar org-jira-issues-mode-keymap (make-sparse-keymap)
	"Key bindings for org-jira-issues-mode.")

(define-minor-mode org-jira-issues-mode
	"Minor mode for org-jira issues buffer."
	nil " Issues" org-jira-issues-mode-keymap
	(org-set-local
	 'header-line-format
	 "Edit: 'C-c e', Comment: 'C-c c', Refresh: 'C-c C-r', New: 'C-c C-n', Resolve: 'C-c C-v', Assign: 'C-c C-a'."))
;; (define-key org-jira-issues-mode-keymap
;; 	(kbd "C-c e") 'org-jira-edit-current-issue)
(define-key org-jira-issues-mode-keymap
	(kbd "C-c c") 'org-jira-comment-current-issue)
(define-key org-jira-issues-mode-keymap
	(kbd "C-c C-a") 'org-jira-assign-current-issue)
(define-key org-jira-issues-mode-keymap
	(kbd "C-c C-r") 'org-jira-refresh-current-buffer)
;; (define-key org-jira-issues-mode-keymap
;; 	(kbd "C-c C-n") 'org-jira-new-issue)
;; (define-key org-jira-issues-mode-keymap
;; 	(kbd "C-c C-v") 'org-jira-change-state-current-issue)


(defvar org-jira-input-mode-keymap (make-sparse-keymap)
	"Key bindings for org-jira-input-mode.")

(define-minor-mode org-jira-input-mode
	"Minor mode for miscellaneous org-jira input buffers."
	nil " OrgJira Input" org-jira-input-mode-keymap
	(org-set-local
	 'header-line-format
	 "OrgJira input buffer.  Done editing 'C-c C-c', discard 'C-c C-k'."))
(define-key org-jira-input-mode-keymap
	(kbd "C-c C-c") 'org-jira-continue-from-input-buffer)
(define-key org-jira-input-mode-keymap
	(kbd "C-c C-k") 'org-jira-discard-from-input-buffer)


(defun org-jira-set-host-url (url)
	"Set the Jira Host URL when the user has failed to configure org-jira properly."
	(interactive
	 (list (read-string "Jira Host URL: ")))
	(setq jira-host-url url))

(defun org-jira-load-wsdl ()
	"Load the Jira WSDL."
	(unless org-jira-host-url
		(call-interactively 'org-jira-set-host-url))
	(setq org-jira-wsdl
				(soap-load-wsdl-from-url (concat org-jira-host-url "/rpc/soap/jirasoapservice-v2?wsdl"))))

(defun org-jira-authenticate (username password)
	"Authenticate with the Jira SOAP service."
	(unless org-jira-wsdl
		(org-jira-load-wsdl))
	(interactive
	 (list (if org-jira-username
						 org-jira-username
					 (setq org-jira-username (read-string "Jira Username: "))
					 org-jira-username)
				 (read-passwd "Jira Password: ")))
	(condition-case nil
			(setq org-jira-auth-token
						(car (soap-invoke org-jira-wsdl org-jira-service "login" username password)))
		(soap-error
		 (display-message-or-buffer "Unable to log in to Jira!"))))

(defun org-jira-soap-call (message-name &rest message-params)
	"Call a method on the remote Jira server."
	(unless org-jira-auth-token
		(call-interactively 'org-jira-authenticate))
	(condition-case nil
			(apply 'soap-invoke
						 org-jira-wsdl 
						 org-jira-service
						 message-name
						 org-jira-auth-token
						 message-params)
		(soap-error
		 (call-interactively 'org-jira-authenticate)
		 (apply 'soap-invoke
						org-jira-wsdl 
						org-jira-service
						message-name
						org-jira-auth-token
						message-params))))

(defun org-jira-load-filters/ ()
	"Helper function to load filters."
	(setq org-jira-filters
				(car (org-jira-soap-call "getFavouriteFilters"))))

(defun org-jira-load-filters ()
	(interactive)
	(setq org-jira-load-filters nil)
	(condition-case nil
			(org-jira-load-filters/)
		(soap-error
		 (setq org-jira-auth-token nil)
		 (call-interactively 'org-jira-authenticate)
		 (org-jira-load-filters/)))
	(unless org-jira-load-filters
		(display-message-or-buffer "Unable to load filters!")))

(defun org-jira-issues-from-jql (jql max-issues)
	"Given a JQL query, return the matching issues up to max-issues issues."
	(condition-case nil
			(car (org-jira-soap-call "getIssuesFromJqlSearch"
															 jql
															 max-issues))))

(defun org-jira-fetch-my-issues (&optional count)
	"Fetch all Jira issues assigned to the current user."
	(unless (numberp count)
		(setq count org-jira-issues-limit))
	(org-jira-issues-from-jql "assignee = currentUser()" count))

(defun org-jira-wrap-line-list (string line-width)
	"Convert a string to a list of lines no wider than the specified width.
Line breaks will be inserted between words.  Words that are longer than
the specified width will be on a line by themselves, exceeding the line width
constraint."
	(let ((words (split-string string))
				(lines '())
				(line ""))
		(while (> (length words) 0)
			(let ((word (pop words)))
				(if (<= (+ (length word) (length line) 1) line-width)
						(setq line (concat line (if (> (length line) 0) " " "") word))
					(setq lines (append lines (list line)))
					(setq line word))))
		(setq lines (append lines (list line)))
		lines))

(defun org-jira-create-comment (key body)
	"Add a new comment to an issue."
	(interactive
	 (list (read-string "Issue Key: ")
				 (read-string "Comment: ")))
	(condition-case nil
			(car (org-jira-soap-call "addComment" key `((body . ,body))))))

(defun org-jira-assign-issue (key assignee)
	"Assign an issue to a user."
	(interactive
	 (list
		(read-string "Issue Key: ")
		(read-string "Assign To: ")))
	(condition-case nil
			(org-jira-soap-call "updateIssue"
													key
													(vector `((id . "assignee") (values . ,(vector assignee)))))))

(defun org-jira-issue-comments (key)
	"Fetch the comments list for a given issue key."
	(condition-case nil
			(car (org-jira-soap-call "getComments" key))))

(defun org-jira-org-properties-from-alist (alist
																					 indent-by
																					 &optional excludes
																					 &key prefix)
	"Convert an alist into a set of org properties, indented to the right level."
	(unless (listp excludes)
		(setq excludes '()))
	(setq prefix
				(concat org-jira-prop-prefix "-" (if (stringp prefix) (concat prefix "-") "")))
	(let ((line-indent (make-string indent-by ? )))
		(concat line-indent ":PROPERTIES:\n"
						(mapconcat (lambda (p) (concat line-indent ":" prefix
																					 (upcase (symbol-name (car p)))
																					 ": " (cdr p)))
											 (delq nil (mapcar (lambda (p) (and (not (or (memq (car p) excludes)
																																	 (not (stringp (cdr p)))))
																													p))
																				 alist))
											 "\n")
						"\n"
						line-indent ":END:\n")))

(defun org-jira-org-from-issue-comment (comment &optional depth)
	"Produce an org entry from a Jira issue comment."
	(unless (numberp depth)
		(setq depth 2))
	(let ((author (cdr (assoc 'author comment)))
				(date (cdr (assoc 'created comment)))
				(body (cdr (assoc 'body comment)))
				(line-indent (make-string (+ depth 1) ? )))
		(unless author
			(setq author "nobody"))
		(unless date
			(setq date "sometime"))
		(concat (make-string depth ?*) " " org-jira-comment-keyword " " author " spake at " date "\n"
						(org-jira-org-properties-from-alist comment
																								(+ depth 1)
																								'(body)
																								:prefix "COMMENT")
						(mapconcat (lambda (x) (concat line-indent x))
											 (org-jira-wrap-line-list body (- 80 (+ depth 1)))
											 "\n")
						(if (> (length body) 0) "\n\n" "\n"))))

(defun org-jira-org-from-issue (issue &optional depth)
	"Produce an org entry from a Jira issue."
	(unless (numberp depth)
		(setq depth 1))
	(let ((summary (cdr (assoc 'summary issue)))
				(description (cdr (assoc 'description issue)))
				(line-indent (make-string (+ depth 1) ? )))
		(unless (stringp description)
			(setq description ""))
		(unless (stringp summary)
			(setq summary ""))
		(concat (make-string depth ?*) " " org-jira-issue-keyword " " summary "\n"
						(org-jira-org-properties-from-alist issue
																								(+ depth 1)
																								'(summary
																									description
																									customFieldValues)
																								:prefix "ISSUE")
						(mapconcat (lambda (x) (concat line-indent x))
											 (org-jira-wrap-line-list description (- 80 (+ depth 1)))
											 "\n")
						(if (> (length description) 0) "\n\n" "\n"))))

(defun org-jira-fill-buffer-with-issues (issues &optional buffer)
	(unless buffer
		(setq buffer (current-buffer)))
	(save-excursion
		(set-buffer buffer)
		(erase-buffer)
		(while (> (length issues) 0)
			(let* ((issue (pop issues))
						 (issue-key (cdr (assoc 'key issue))))
				(insert (org-jira-org-from-issue issue))
				(mapc (lambda (comment) (insert (org-jira-org-from-issue-comment comment)))
							(org-jira-issue-comments issue-key))))))


(defun org-jira-org-buffer-from-issues (issues)
	"Produce an org buffer from a list of Jira issues.  Destroys issues list."
	(set-buffer (generate-new-buffer "org-jira-issues"))
	(org-mode)
	(org-jira-fill-buffer-with-issues issues)
	(org-jira-issues-mode)
	(org-pop-to-buffer-same-window (current-buffer)))

(defun org-jira-org-buffer-from-jql (jql)
	"Display an org buffer containing the results of a JQL query."
	(interactive (list (read-string "Query: ")))
	(org-jira-org-buffer-from-issues (org-jira-issues-from-jql jql org-jira-issues-limit))
	(org-set-local 'org-jira-jql-query jql))

(defun org-jira-current-issue-key ()
	"Key the Jira Issue Key for the issue the point is currently in."
	(interactive)
	(let ((key-prop (concat org-jira-prop-prefix "-ISSUE-KEY")))
		(save-excursion
			(org-entry-get (point-marker)
										 key-prop
										 t))))
(defun org-jira-current-issue-add-comment (comment)
	"Add a comment to the current issue."
	(org-jira-create-comment (org-jira-current-issue-key)
													 comment))

(defun org-jira-continue-with-input-buffer (cont-fun &optional template)
	"Throws up an input buffer, provides the user some instructions, and runs the
provided cont-fun when the user signals completion.  Returns to the current
buffer at the end."
	(let ((old-buffer (current-buffer))
				(my-buffer (generate-new-buffer "*OrgJira Input Buffer*")))
		(set-buffer my-buffer)
		(when template
			(insert template))
		(org-jira-input-mode)
		(org-set-local 'result-fun cont-fun)
		(org-set-local 'orig-buffer old-buffer)
		(org-pop-to-buffer-same-window my-buffer)))

(defun org-jira-continue-from-input-buffer ()
	"Resume processing after receiving input from the input buffer."
	(interactive)
	(let ((my-buffer (current-buffer)))
		(funcall result-fun (buffer-string))
		(set-buffer orig-buffer)
		(org-pop-to-buffer-same-window (current-buffer))
		(kill-buffer my-buffer)))

(defun org-jira-discard-from-input-buffer ()
	"Discard input received from the input buffer."
	(interactive)
	(let ((my-buffer (current-buffer)))
		(set-buffer orig-buffer)
		(org-pop-to-buffer-same-window (current-buffer))
		(kill-buffer my-buffer)))

				

(defun org-jira-comment-current-issue (&optional comment)
	"Add a comment to the current issue."
	(interactive)
	(if comment
			(org-jira-current-issue-add-comment comment)
		(let ((issue-key (org-jira-current-issue-key)))
			(org-jira-continue-with-input-buffer
			 `(lambda (x)
					(org-jira-create-comment ,issue-key x))))))
																	

(defun org-jira-assign-current-issue (user)
	"Assign the current issue to someone."
	(interactive (list (read-string "Assign To: ")))
	(org-jira-assign-issue (org-jira-current-issue-key) user))

(defun org-jira-refresh-current-buffer ()
	"Refresh the content of the current buffer by repeating the JQL query that
generated the issues listed."
	(interactive)
	(org-jira-fill-buffer-with-issues (org-jira-issues-from-jql org-jira-jql-query org-jira-issues-limit)))

(defun org-jira-new-issue ()
	"Create a new Jira issue by editing the issue template in a buffer."
	(interactive)
	(org-jira-continue-with-input-buffer
	 (lambda (x)
		 (org-jira-create-issue x))
	 (org-jira-org-from-issue org-jira-default-new-issue)))
		 
(provide 'org-jira)

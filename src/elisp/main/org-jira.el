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

(defgroup org-jira nil
	"Jira/Org-Mode Integration"
	:group 'tools)

(defcustom org-jira-host-url
	"http://jira.atlassian.com"
	"The base URL for your Jira service."
	:type 'string
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

(defvar org-jira-wsdl nil)
(defvar org-jira-filters nil)
(defvar org-jira-resolutions nil)
(defvar org-jira-auth-token nil)
(defvar org-jira-service "jirasoapservice-v2")

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
	 (list (unless org-jira-username
					 (read-string "Jira Username: ")
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
		(setq count 10000))
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
						(setq line (concat line " " word))
					(setq lines (append lines (list line)))
					(setq line word))))
		(setq lines (append lines (list line)))
		lines))

(defun org-jira-issue-comments (key)
	"Fetch the comments list for a given issue key."
	(condition-case nil
			(car (org-jira-soap-call "getComments" key))))

(defun org-jira-org-properties-from-alist (alist indent-by &optional excludes)
	"Convert an alist into a set of org properties, indented to the right level."
	(unless (listp excludes)
		(setq excludes '()))
	(let ((line-indent (make-string indent-by ? )))
		(concat line-indent ":PROPERTIES:\n"
						(mapconcat (lambda (p) (concat line-indent ":JIRA-"
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
		(concat (make-string depth ?*) " " author " spake at " date "\n"
						(org-jira-org-properties-from-alist comment
																								(+ depth 1)
																								'(body))
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
		(concat (make-string depth ?*) " JIRA " summary "\n"
						(org-jira-org-properties-from-alist issue (+ depth 1) '(summary
																																		description
																																		customFieldValues))
						(mapconcat (lambda (x) (concat line-indent x))
											 (org-jira-wrap-line-list description (- 80 (+ depth 1)))
											 "\n")
						(if (> (length description) 0) "\n\n" "\n"))))

(defun org-jira-org-buffer-from-issues (filename issues)
	"Produce an org file from a list of Jira issues.  Destroys issues list."
	(set-buffer (generate-new-buffer "org-jira-issues"))
	(with-current-buffer
			(local-set-key (kbd "C-c C-c") 'org-jira-send-updates)
		(local-set-key (kbd "C-c C-n") 'org-jira-new-issue)
		(local-set-key (kbd "C-c C-m") 'org-jira-new-message)
		(local-set-key (kbd "C-c C-k") 'kill-this-buffer)
		(local-set-key (kbd "C-c C-r") 'org-jira-resolve)
		(local-set-key (kbd "C-c c") 'org-jira-change-state)
		(org-mode)
		(while (> (length issues) 0)
			(let* ((issue (pop issues))
						 (issue-key (cdr (assoc 'key issue))))
				(message 
				 (insert (org-jira-org-from-issue issue))
				 (mapc (lambda (comment) (insert (org-jira-org-from-issue-comment comment)))
							 (org-jira-issue-comments issue-key)))))))

(provide 'org-jira)
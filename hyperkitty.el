;; -*- lexical-binding: t -*-
(require-package 'request)

;; (defcustom hyperkitty-base-url nil
;;   "Base URL for Hyperkitty server."
;;   :type str
;;   :group 'hyperkitty)

(setf debug-on-error t)
(setf lexical-binding t)

(setq hyperkitty-base-url  "https://lists.mailman3.org/archives")

(defun hyperkitty-lists-url (base-url)
  (concat base-url "/api/lists/"))

(defun hyperkitty-threads-url (base-url mlist)
  (concat base-url "/api/list/" mlist "/threads"))


(defun get-hk-base-url ()
  "Prompt User to get the base URL for hyperkitty."
  (interactive)
  (read-string "Enter Hyperkitty URL:"))


(defun get-json (url success-func)
  "Download and get all MailingLists from a server."
  (interactive)
  (request
   url
   :parser 'json-read
   :success
   (cl-function (lambda (&key response &allow-other-keys)
				  (funcall success-func response)))))


(defun print-mailinglist-response (response)
  "Print each element MailingLists from the response."
  (with-current-buffer "*scratch*"
    (insert (format "URL: %s, Status: %s, Data: %s"
					(request-response-url response)
					(request-response-status-code response)
					(mapcar 'print-mailinglist (request-response-data response))))))


(defun print-mailinglist (mlist)
  "Get an association lists and prints the display name,
name and description of the list."
  (format "\nName: %s \nAddress: %s \nDescription: %s \n"
		  (assoc-default 'display_name mlist)
		  (assoc-default 'name mlist)
		  (assoc-default 'description mlist)))


(defun print-threads-response (response)
  (with-current-buffer "*scratch*"
	(insert (format "%s" (mapcar 'print-thread (request-response-data response))))))


(defun print-thread (thread)
  (format "Subject: %s \nReplies: %s"
		  (assoc-default 'subject thread)
		  (assoc-default 'replies_count thread)))


(defun choose-mailinglist (response)
  "Ask user to choose a Mailinglist from the response."
  (ido-completing-read
   "Select from list: "
   (mapcar
	(lambda (arg) (assoc-default 'name arg))
	(request-response-data response))))


(defun choose-mailinglist-and-get-threads (response)
  "Choose mailinglist and get their threads."
  (let* ((mlist (choose-mailinglist response))
		 (threads-url (hyperkitty-threads-url hyperkitty-base-url mlist)))
	(get-json threads-url 'print-threads-response)))


(get-json
 (hyperkitty-lists-url hyperkitty-base-url)
 'choose-mailinglist-and-get-threads)

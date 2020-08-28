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

(defun hyperkitty-thread-emails-url (threads-url)
  (concat threads-url "/emails"))


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

(defun get-response-entries (response)
  (assoc-default 'results (request-response-data response)))


(defun print-mailinglist-response (response)
  "Print each element MailingLists from the response."
  (with-current-buffer "*scratch*"
    (insert (format "URL: %s, Status: %s, Data: %s"
					(request-response-url response)
					(request-response-status-code response)
					(mapcar 'print-mailinglist (get-response-entries response))))))


(defun print-mailinglist (mlist)
  "Get an association lists and prints the display name,
name and description of the list."
  (format "\nName: %s \nAddress: %s \nDescription: %s \n"
		  (assoc-default 'display_name mlist)
		  (assoc-default 'name mlist)
		  (assoc-default 'description mlist)))


(defun get-threads-response (response)
  (mapcar (lambda (arg) (list (assoc-default 'url arg)
							  (vector (assoc-default 'subject arg)
									  (assoc-default 'date_active arg))))
		  (get-response-entries response)))


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
	(get-response-entries response))))


(defun choose-mailinglist-and-get-threads (response)
  "Choose mailinglist and get their threads."
  (let* ((mlist (choose-mailinglist response))
		 (threads-url (hyperkitty-threads-url hyperkitty-base-url mlist)))
	(get-json threads-url (apply-partially 'print-threads-table mlist))))


(defun get-thread-emails ()
  "Get Emails for the thread of current line."
  (interactive)
  (get-json (tabulated-list-get-id) 'print-email-response)
  )


(defun print-email-response (response)
  (pop-to-buffer "Thread Emails")
  (insert (get-response-entries response)))


(defvar threads-mode-map nil "Keymap for 'threads-mode-map'")
(progn
  (setq threads-mode-map (make-sparse-keymap))
  (define-key threads-mode-map (kbd "<RET>") 'get-thread-emails)
  )


(define-derived-mode threads-mode tabulated-list-mode "threads-mode"
  "Major mode for MailingList threads."
  (setq tabulated-list-format [("Subject" 100 nil)
                               ("Last Active Date" 15 t)])
  (setq tabulated-list-padding 2)
  (setq tabulated-list-sort-key (cons "Last Active Date" nil))
  (tabulated-list-init-header))


(defun print-current-line-id ()
   (message (concat "current line ID is: " (tabulated-list-get-id))))


(defun print-threads-table (mlist response)
  (interactive)
  (pop-to-buffer (format "*%s*" mlist) nil)
  (threads-mode)
  (setq tabulated-list-entries (get-threads-response response))
  (tabulated-list-print t))



(defun hyperkitty ()
  "This is the primary entrypoint to hyperkitty.el

It fetches the Hyperkitty API from the `hyperkitty-base-url'
variable and let's user choose one of the mailing list and then
opens a new buffer with a list of threads for that MailingList.
"
  (interactive)
  (get-json
   (hyperkitty-lists-url hyperkitty-base-url)
   'choose-mailinglist-and-get-threads))

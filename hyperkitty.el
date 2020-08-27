(require-package 'request)

;; (defcustom hyperkitty-base-url nil
;;   "Base URL for Hyperkitty server."
;;   :type str
;;   :group 'hyperkitty)

(defun get-hk-base-url ()
  "Prompt User to get the base URL for hyperkitty."
  (interactive)
  (read-string "Enter Hyperkitty URL:"))



(defun get-mailinglists (&optional base-url)
"Download and get all MailingLists from a server."
(interactive)
(request
 "https://lists.mailman3.org/archives/api/lists"
 :parser 'json-read
 :success
 (cl-function (lambda (&key response &allow-other-keys)
                (with-current-buffer "*scratch*"
                  (insert (format "URL: %s, Status: %s, Data: %s"
								  (request-response-url response)
								  (request-response-status-code response)
								  (mapcar 'print-mailinglist (request-response-data response)))))))))


(defun print-mailinglist (mlist)
"Get an association lists and prints the display name,
name and description of the list."
(format "\nName: %s \nAddress: %s \nDescription: %s \n"
		(assoc-default 'display_name mlist)
		(assoc-default 'name mlist)
		(assoc-default 'description mlist))
)

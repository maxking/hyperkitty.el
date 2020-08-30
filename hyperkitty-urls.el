(defun hyperkitty-lists-url (base-url)
  "Get a list of all MailingLists from API.

Currently, this does not handle pagination and simply returns the
default URL without pagination.
"
  (concat base-url "/api/lists/"))

(defun hyperkitty-threads-url (base-url mlist)
  "Get a list of all threads for a MailingList.

Currently, this does not handle pagination and simply returns the
default URL without pagination.
"
  (concat base-url "/api/list/" mlist "/threads"))

(defun hyperkitty-thread-emails-url (threads-url)
  "Get all emails for the given thread URL.

This does not handle pagination currently.
"
  (concat threads-url "/emails"))


(defun hyperkitty-get-base-url ()
  "Prompt User to get the base URL for hyperkitty."
  (interactive)
  (read-string "Enter Hyperkitty URL:"))


(provide 'hyperkitty-urls)

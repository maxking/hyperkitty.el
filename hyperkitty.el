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


(defun get-json (url success-func)
  "Call the given `url' and call provided call function on success.

We currently don't have a good error handling."
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
  "Print each element MailingLists from the response.

Handler for HTTP response for all the lists API. It simply prints
the list of MailingList.

TODO: This should accept a handler which should handle each entry
of the response."
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
  "Given a HTTP response, return a list that tablulated-list-mode.

It expects data in the form of:
(<thread-url> [(<subject> <date>)
               (<subject2> <date2>)
               ...])

It also expects the list to be paginated with simple
PageNumberPagination from Django Rest Framework.
"
  (mapcar (lambda (arg) (list (assoc-default 'url arg)
                              (vector (assoc-default 'subject arg)
									  (number-to-string (assoc-default 'replies_count arg))
                                      (assoc-default 'date_active arg))))
          (get-response-entries response)))


(defun choose-mailinglist (response)
  "Ask user to choose a Mailinglist from the /lists API."
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
  (get-json
   (hyperkitty-thread-emails-url (tabulated-list-get-id))
   (apply-partially 'print-emails-response (elt (tabulated-list-get-entry) 0))))


(defun print-emails-response (subject response)
  "Print all the Emails of a thread in a new Buffer.

Create a new buffer, named after the subject of the email and
print all the emails in that new buffer. Each Email's content is
fetched individual since the entries only contain the metadata
about the Email.
"
  (pop-to-buffer (format "*%s*" subject))
  (erase-buffer)
  (mail-mode)
  (mapcar 'print-email (get-response-entries response)))


(defun print-email (email)
  "Fetch and print a single email to the current buffer.

Given a metadata object for Email, fetch the actual contents of
the Email and print it to the current buffer.
"
  (get-json
   (assoc-default 'url email)
   (lambda (response)
     (let ((anemail (request-response-data response)))
       (insert (format "From: %s\nSubject: %s\nDate: %s:\n\n%s\n\n"
                       (assoc-default 'sender_name anemail)
                       (assoc-default 'subject anemail)
                       (assoc-default 'date anemail)
                       (assoc-default 'content anemail)))))))



;; Keyboard map for threads-mode.
(defvar threads-mode-map nil "Keymap for 'threads-mode-map'")
(progn
  (setq threads-mode-map (make-sparse-keymap))
  (define-key threads-mode-map (kbd "<RET>") 'get-thread-emails)
  )


(define-derived-mode threads-mode tabulated-list-mode "threads-mode"
  "Major mode for MailingList threads.

It presents a list of threads in a tabular format with three
columns, Subject, Reply and Last Active date.
"
  (setq tabulated-list-format [("Subject" 80 nil)
							   ("Reply" 5 nil)
                               ("Last Active" 15 t)])
  (setq tabulated-list-sort-key (cons "Last Active" nil))
  (tabulated-list-init-header))


(defun print-threads-table (mlist response)
  "Print the whole threads table for a given MailingList.

Create a new buffer, named after the MailingList and switch to
threads-mode. Finally, display all the threads from the response.
"
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

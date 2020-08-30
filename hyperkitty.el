;; -*- lexical-binding: t -*-
(require-package 'request)

(require 'hyperkitty-thread)
(require 'hyperkitty-urls)
(require 'hyperkitty-email)

;; (defcustom hyperkitty-base-url nil
;;   "Base URL for Hyperkitty server."
;;   :type str
;;   :group 'hyperkitty)

(setf debug-on-error t)
(setf lexical-binding t)

(setq hyperkitty-base-url  "https://lists.mailman3.org/archives")


;;; HTTP fetch utilities.
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


;;; Test function to print all the mailing list.
(defun print-mailinglist-response (response)
  "Print each element MailingLists from the response.

Handler for HTTP response for all the lists API. It simply prints
the list of MailingList."
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

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
  (thread-emails-mode)
  (read-only-mode)
  (setq outline-regexp "From: ")
  (let ((inhibit-read-only t))
        (insert (propertize
                 (format "%s\n" subject)
                 'font-lock-face 'bold
                 'height 200)))
  (mapcar 'print-email (reverse (get-response-entries response))))


(defun print-email (email)
  "Fetch and print a single email to the current buffer.

Given a metadata object for Email, fetch the actual contents of
the Email and print it to the current buffer.
"
  (get-json
   (assoc-default 'url email)
   (lambda (response)
     (let ((anemail (request-response-data response))
           (inhibit-read-only t))
       (insert (print-email-headers anemail))
       (maybe-print-email-attachments anemail)
       (insert (format "\n%s\n\n" (assoc-default 'content anemail))))
     (outline-hide-entry))))


(defun print-email-headers (anemail)
  (format
   "From: %s <%s>\nMessage-ID: %s\nSubject: %s\nDate: %s:\n"
   (assoc-default 'sender_name anemail)
   (assoc-default 'address (assoc-default 'sender anemail))
   (assoc-default 'message_id anemail)
   (assoc-default 'subject anemail)
   (assoc-default 'date anemail)))

(defun maybe-print-email-attachments (anemail)
  (let ((attachments (assoc-default 'attachments anemail)))
    (if (> (length attachments) 0)
        (progn
          (insert "Attachments: ")
          (insert
           (mapconcat
            (lambda (arg) (format "%s(%sKB)" (assoc-default 'name arg) (assoc-default 'size arg)))
            attachments
            ", "))
          (insert "\n")))))


(define-derived-mode thread-emails-mode outline-mode "thread-emails-mode"
  "Minor mode to simulate buffer local keybindings."
  (setq font-lock-defaults '(email-highlights)))


(defun hyperkitty-outline-toggle ()
  "Show/Hide the current outline entry we are on."
  (interactive)
  (if (outline-invisible-p (line-end-position))
	  (outline-show-entry)
	(outline-hide-entry)))

(defun hyperkitty-kill-current-buffer ()
  "Kill the current buffer."
  (interactive)
  (kill-buffer (current-buffer)))


(defvar thread-emails-mode-map nil "Keymap for 'thread-emails-mode'")
(progn
  (setq thread-emails-mode-map (make-sparse-keymap))
  (define-key thread-emails-mode-map (kbd "<RET>") 'hyperkitty-outline-toggle)
  (define-key thread-emails-mode-map (kbd "TAB") 'hyperkitty-outline-toggle)
  (define-key thread-emails-mode-map (kbd "q") 'hyperkitty-kill-current-buffer))


(setq email-highlights
      '(("From:\\|Date:\\|Subject:\\|Message-ID:\\|Attachments:" . font-lock-keyword-face)
        (">.*" . font-lock-comment-face)))

(provide 'hyperkitty-email)

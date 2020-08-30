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
  (setq outline-regexp "From: ")
  (insert (propertize
		   (format "%s\n" subject)
		   'font-lock-face 'bold
		   'height 200))
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


(define-derived-mode thread-emails-mode outline-mode "thread-emails-mode"
  "Minor mode to simulate buffer local keybindings."
  (setq font-lock-defaults '(email-highlights)))


(defun hyperkitty-outline-toggle ()
  (interactive)
  (if (outline-invisible-p (line-end-position))
	  (outline-show-entry)
	(outline-hide-entry)))


(defvar thread-emails-mode-map nil "Keymap for 'thread-emails-mode'")
(progn
  (setq thread-emails-mode-map (make-sparse-keymap))
  (define-key thread-emails-mode-map (kbd "<RET>") 'hyperkitty-outline-toggle)
  (define-key thread-emails-mode-map (kbd "TAB") 'hyperkitty-outline-toggle))


(setq email-highlights
      '(("From:\\|Date:\\|Subject:" . font-lock-keyword-face)
        (">.*" . font-lock-comment-face)))

(provide 'hyperkitty-email)

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
  (setq tabulated-list-sort-key (cons "Last Active" t))
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


(provide 'hyperkitty-thread)

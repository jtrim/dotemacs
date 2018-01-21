;;;
;; Helper functions

(defun parse-database-url (database-url)
  (let ((parsed-url
         (mapcar (lambda (s) (string-trim s))
                 (s-split "[@:/]"
                          (s-replace "postgres://" "" database-url))))
        (h (make-hash-table)))
    (puthash :user (first parsed-url) h)
    (puthash :password (second parsed-url) h)
    (puthash :host (third parsed-url) h)
    (puthash :port (first (last (butlast parsed-url))) h)
    (puthash :database (first (last parsed-url)) h)
    h))

(defun pivotal-tracker-number-from-branch-list (branch-list)
  (first (s-split "-" (string-trim (first (last (s-split " " (seq-find (lambda (line) (s-starts-with? "*" line)) (s-split "\n" branch-list)))))))))

(defun pivotal-tracker-number-from-current-branch ()
  (pivotal-tracker-number-from-branch-list (shell-command-to-string "git branch")))

(defun pascal-case-name-from-file-name (filename)
  (let ((terminus (f-filename filename)))
    (s-join ""
            (mapcar 'capitalize
                    (s-split "[_]" (first (s-split "[.]" terminus)))))))

(defun adjust-number-at-point (adj-fn)
  (interactive)
  (skip-chars-backward "0-9")
  (or (looking-at "[0-9]+")
      (error "No number at point"))
  (replace-match (number-to-string (apply adj-fn (list (string-to-number (match-string 0)) 1)))))

(defun increment-number-at-point ()
  (interactive)
  (adjust-number-at-point '+))

(defun decrement-number-at-point ()
  (interactive)
  (adjust-number-at-point '-))

(defmacro -> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append (list (car form) result)
                           (cdr form))))))

(defmacro ->> (&rest body)
  (let ((result (pop body)))
    (dolist (form body result)
      (setq result (append form (list result))))))

(defun pop-tag ()
  (interactive)
  (xref-pop-marker-stack)
  (funcall (key-binding (kbd "C-l"))))

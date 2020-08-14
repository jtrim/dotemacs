;;;
;; helper functions

(defun parse-database-url (database-url)
  (let ((parsed-url
         (mapcar (lambda (s) (string-trim s))
                 (s-split "[@:/]"
                          (s-replace "postgres://" "" database-url))))
        (h (make-hash-table)))
    (puthash :user (cl-first parsed-url) h)
    (puthash :password (cl-second parsed-url) h)
    (puthash :host (cl-third parsed-url) h)
    (puthash :port (cl-first (last (butlast parsed-url))) h)
    (puthash :database (cl-first (last parsed-url)) h)
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

(defun kill-ring-save-line ()
  (interactive))

(defun open-config-file ()
  (interactive)
  (split-window-right)
  (find-file (s-concat user-emacs-directory "src/config.el")))

(defun resume-last-search-buffer ()
  "open last helm-ag or hgrep buffer."
  (interactive)
  (cond ((get-buffer "*helm ag results*")
         (switch-to-buffer-other-window "*helm ag results*"))
        ((get-buffer "*helm-ag*")
         (helm-resume "*helm-ag*"))
        ((get-buffer "*hgrep*")
         (switch-to-buffer-other-window "*hgrep*"))
        (t
         (message "No previous search buffer found"))))

(defun change-global-font-size (delta)
  (let ((current-size (font-get default-global-font-spec :size)))
    (font-put default-global-font-spec :size (+ current-size delta))
    (set-face-attribute 'default nil :font default-global-font-spec)))

(defun increase-global-font-size ()
  (interactive)
  (change-global-font-size 1))

(defun decrease-global-font-size ()
  (interactive)
  (change-global-font-size -1))

(defun open-org-file ()
  (interactive)
  (split-window-right)
  (find-file (expand-file-name "~/Dropbox/notes.org")))

(defun ruby-change-to-multi-line-keyword-args (from to)
  (interactive (list (region-beginning) (region-end)))
  (let (s outputS)
    (setq s       (buffer-substring-no-properties from to))
    (setq outputS (replace-regexp-in-string ", " ",\n" s))

    (save-excursion
      (delete-region from to)
      (goto-char from)
      (insert outputS)
      (indent-region from to))))

(defvar focus-mode-margin-width 120)
(defun focus-mode--go-focus ()
  (interactive)
  (focus-mode--set-focus-margins focus-mode-margin-width)
  (if (derived-mode-p 'prog-mode)
      (visual-line-mode -1)
    (visual-line-mode 1))
  (linum-mode -1)
  (set-window-buffer nil (current-buffer)))

(defun focus-mode--no-focus ()
  (interactive)
  (focus-mode--set-focus-margins 0)
  (linum-mode 1)
  (visual-line-mode -1)
  (set-window-buffer nil (current-buffer)))

(defun focus-mode--set-focus-margins (margin-width)
  (setq left-margin-width margin-width)
  (setq right-margin-width margin-width))

(define-minor-mode focus-mode
  "todo - make a docstring"
  :init-value nil
  :lighter "•`_´•"
  (progn
    (if focus-mode
        (focus-mode--go-focus)
      (focus-mode--no-focus))))

;; Apparently this doesn't exist?
;; (add-variable-watcher 'focus-mode-margin-width (lambda (sym newval op wh) (focus-mode--set-focus-margins newval)))

(defun projectile-regen-etags ()
  (interactive)
  (projectile-with-default-dir (projectile-project-root)
    (shell-command "ctags -e")))

(defun getmd5 ()
  (interactive)
  (kill-new (md5 (current-buffer) (region-beginning) (region-end)) nil))

(defun upcase-sql-keywords ()
  (interactive)
  (save-excursion
    (dolist (keywords sql-mode-postgres-font-lock-keywords)
      (goto-char (point-min))
      (while (re-search-forward (car keywords) nil t)
        (goto-char (+ 1 (match-beginning 0)))
        (when (eql font-lock-keyword-face (face-at-point))
          (backward-char)
          (upcase-word 1)
          (forward-char))))))

(defun kill-ring-save-sexp-forward ()
  (interactive)
  (let ((start (point)))
    (progn
      (forward-sexp)
      (kill-ring-save start (point)))))

(defun revert-all-buffers-dangerously ()
  "Refreshes all open buffers from their respective files."
  (interactive)
  (dolist (buf (buffer-list))
    (with-current-buffer buf
      (when (and (buffer-file-name) (file-exists-p (buffer-file-name)))
        (revert-buffer t t t) )))
  (message "Refreshed open files."))

(defun sp-copy-inside-parent-sexp ()
  (interactive)
  (save-excursion
    (sp-backward-up-sexp)
    (sp-down-sexp)
    (let ((start (point)))
      (sp-up-sexp)
      (sp-backward-down-sexp)
      (kill-ring-save start (point)))))

(defun set-noop-indent-line-function ()
  (setq-local indent-line-function
              (lambda () (interactive))))

(defun open-line-in-github ()
  (interactive)
  (let ((repo (magithub-repo)))
    (let ((base-url (let-alist repo .html_url))
          (current-file (magit-current-file))
          (current-commit (magit-rev-parse "HEAD"))
          (current-line (number-to-string (line-number-at-pos))))
      (shell-command (concat "open " base-url "/tree/" current-commit "/" current-file "#L" current-line)))))

(defun bury-compile-buffer-if-successful (buffer string)
  "Bury a compilation buffer if succeeded without warnings "
  (when (and
         (buffer-live-p buffer)
         (string-match "compilation" (buffer-name buffer))
         (string-match "finished" string)
         (not rspec-keep-buffer-active)
         (not
          (with-current-buffer buffer
            (goto-char (point-min))
            (search-forward "warning" nil t))))
    (unless (eq (window-buffer (selected-window)) buffer)
      (run-with-timer 0.5 nil
                      (lambda (buf) (delete-window (get-buffer-window buf)))
                      buffer))))

(defun ruby-unalign-region (from to)
  (interactive (list (region-beginning) (region-end)))
  (save-excursion
    (goto-char from)
    (search-forward ":")
    (let ((found-p nil))
      (while (and (< (point) to) (not found-p))
        (let ((current-from (point)))
          (progn
            (search-forward-regexp "[^ ]")
            (if (eq (+ 1 current-from) (point))
                (progn
                  (forward-line)
                  (back-to-indentation))
              (progn
                (backward-char)
                (delete-region current-from (point))
                (insert-char ?\s)
                (forward-line)
                (back-to-indentation)
                (unless
                    (search-forward ":" nil t)
                  (setq found-p t))))))))))

(defun kill-xref-window-and-buffer ()
  (let ((xref-buffer (get-buffer "*xerf*")))
    (progn
      (delete-window (get-buffer-window xref-buffer))
      (kill-buffer xref-buffer))))

;; WIP
;; TODO: look into (helm ...) and helm-source documentation
(defun load-gem-tags ()
  (interactive)
  (let ((ruby-version-string (shell-command-to-string "ruby -e 'print RUBY_VERSION'")))
    (let ((gem-directory-contents (directory-files (concat "~/.gem/ruby/" ruby-version-string "/gems"))))
      (print gem-directory-contents))))

;; Leaving this in place for posterity
;; (defun inf-ruby-console-heroku-staging ()
;;   (interactive)
;;   (inf-ruby-console-run "heroku run rails c -a REDACTED" "heroku console staging"))

(defun current-file-name-to-kill-ring ()
  "Put the current file name on the kill ring"
  (interactive)
  (let ((filename (if (equal major-mode 'dired-mode)
                      default-directory
                    (buffer-file-name))))
    (when filename
      (with-temp-buffer
        (insert filename)
        (clipboard-kill-region (point-min) (point-max)))
      (message filename))))

(defun new-line-above ()
  (interactive)
  (back-to-indentation)
  (newline-and-indent)
  (previous-line)
  (indent-according-to-mode))

(defun uniquify-buffer-lines ()
    "Remove duplicate adjacent lines in the current buffer."
    (interactive)
    (uniquify-region-lines (point-min) (point-max)))

(defun uniquify-region-lines (beg end)
    "Remove duplicate adjacent lines in region."
    (interactive "*r")
    (save-excursion
      (goto-char beg)
      (while (re-search-forward "^\\(.*\n\\)\\1+" end t)
        (replace-match "\\1"))))

(defun find-file-at-point-with-line()
  "if file has an attached line num goto that line, ie boom.rb:12"
  (interactive)
  (setq line-num 0)
  (save-excursion
    (search-forward-regexp "[^ ]:" (point-max) t)
    (if (looking-at "[0-9]+")
         (setq line-num (string-to-number (buffer-substring (match-beginning 0) (match-end 0))))))
  (find-file-at-point)
  (if (not (equal line-num 0))
      (goto-line line-num)))

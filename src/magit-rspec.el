(require 'seq)

(defun magit-rspec--convert-path-to-spec-path (path)
  (if (string-suffix-p "_spec.rb" path)
      path
    (rspec-spec-file-for path)))

(defun magit-rspec--changed-file-paths ()
  (mapcar 'car (mapcar (lambda (path) (projectile-expand-paths (list path)))
                       (mapcar 'car (magit-file-status "-uall")))))

(defun magit-rspec--changed-spec-file-paths ()
  (let ((ruby-files (seq-filter (lambda (path) (string-suffix-p ".rb" path)) (magit-rspec--changed-file-paths))))
    (mapcar 'magit-rspec--convert-path-to-spec-path ruby-files)))

(defun magit-rspec-run-changed-files ()
  (interactive)
  (rspec-run-multiple-files (seq-filter 'file-exists-p (magit-rspec--changed-spec-file-paths))))

(provide 'magit-rspec)

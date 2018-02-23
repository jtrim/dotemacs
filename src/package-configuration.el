(require 'package)

(defvar package-list)

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/") t)

(package-initialize)

(unless package-archive-contents
  (package-refresh-contents))

(defun install-and-configure-packages (package-list)
  (dolist (pkg package-list)
    (cond
     ;; Package is plainly listed by name
     ((eq (type-of pkg) 'symbol)
      (unless (package-installed-p pkg)
        (package-install pkg))
      (require pkg)
      (message (concat "Simple installation of package " (symbol-name pkg) " complete.")))

     ;; Package is a pair consisting of a name and a configuration lambda
     ((eq (type-of pkg) 'cons)
      (let ((package-name (car pkg))
            (package-config-lambda (car (cdr pkg))))
        (unless (package-installed-p pkg)
          (package-install package-name))
        (apply package-config-lambda ())
        (message (concat "Complex installation of package " (symbol-name package-name) " complete.")))))))

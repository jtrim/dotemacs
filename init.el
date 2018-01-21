(defun user/init ()
  ;;;
  ;; Bootstrapping

  (setq auto-save-file-name-transforms        `((".*" ,temporary-file-directory t))
	backup-directory-alist                `((".*" . ,temporary-file-directory))
	frame-resize-pixelwise                t
	indent-tabs-mode                      nil
	inhibit-startup-screen                t
	vc-follow-symlinks                    t
	whitespace-style                      '(trailing tabs empty indentation))
  (setq-default line-spacing 2)
  (set-default 'truncate-lines t)

  (add-to-list 'load-path "~/.emacs.d/elisp")
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(font . "Hack-12" ))

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-linum-mode 1)

  (set-face-attribute 'default t :font "Hack-10")

  (global-set-key (kbd "C-c y e") 'yas-expand)
  ;; (global-set-key (kbd "M-m a m") 'helm-imenu)
  (global-set-key (kbd "C-c j") 'join-line)
  (global-set-key (kbd "C-c t") 'pop-tag)
  (global-set-key (kbd "C-c q") 'fill-region)
  (global-set-key (kbd "C-c n a") 'increment-number-at-point)
  (global-set-key (kbd "C-c n x") 'decrement-number-at-point)
  ;; (global-set-key (kbd "C-.") 'er/expand-region)
  (global-set-key (kbd "M-<up>") 'move-text-line-up)
  (global-set-key (kbd "M-<down>") 'move-text-line-down)

  ;;;
  ;; Package

  (require 'package)

  (setq package-list '(company
		       golden-ratio
		       helm
		       helm-ag
		       helm-projectile
		       inf-ruby
		       magit
		       phoenix-dark-pink-theme
		       projectile
		       rspec-mode
		       smart-mode-line
		       smartparens
		       yaml-mode
		       yasnippet))

  (add-to-list 'package-archives
	       '("melpa" . "https://melpa.org/packages/") t)

  (package-initialize)

  (unless package-archive-contents
    (package-refresh-contents))

  (dolist (pkg package-list)
    (unless (package-installed-p pkg)
      (package-install pkg)))

  ;;;
  ;; Package Configuration

  ;;;;
  ;;; Whitespace
  (require 'whitespace)
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;;;;
  ;;; ruby-mode
  (require 'smartparens-ruby)

  ;;;;
  ;;; Theme

  (load-theme 'phoenix-dark-pink t)

  ;;;;
  ;;; Windmove
  (windmove-default-keybindings)

  ;;;;
  ;;; Helm
  (require 'helm)
  (helm-mode)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map [remap list-buffers] 'helm-mini)
  (define-key global-map [remap find-tag]              'helm-etags-select)
  (define-key global-map [remap xref-find-definitions] 'helm-etags-select)
  (define-key helm-find-files-map "\t" 'helm-execute-persistent-action)
  (require 'helm-ag)

  ;;;;
  ;;; Golden Ratio
  (require 'golden-ratio)
  (golden-ratio-mode)

  ;;;;
  ;;; Smartparens
  (require 'smartparens)
  (smartparens-global-strict-mode t)

  ;;;;
  ;;; Modeline config
  (require 'smart-mode-line)
  ;; (sml/setup)

  ;;;;
  ;;; inf-ruby
  (require 'inf-ruby)

  ;;;;
  ;;; rspec-mode
  (require 'rspec-mode)

  ;;;;
  ;;; magit
  (require 'magit)
  (magit-mode)
  (global-set-key (kbd "M-g M-s") 'magit)

  ;;;;
  ;;; projectile
  (require 'projectile)
  (projectile-global-mode)
  (setq projectile-completion-system 'helm)
  (helm-projectile-on)
  (global-set-key (kbd "M-p") 'projectile-find-file)

  ;;;;
  ;;; company-mode
  (require 'company)
  (add-hook 'after-init-hook 'global-company-mode)

  ;;;;
  ;;; yaml-mode
  (require 'yaml-mode))

;;;
;; Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "84e39ed4c552b75e1cb09458c140a9b025598002533456b4c27db31d27e1e0d7" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (helm-ag yaml-mode company magit rspec-mode inf-ruby phoenix-dark-pink-theme molokai-theme inkpot-theme smart-mode-line powerline yasnippet helm helm-ebdb golden-ratio))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

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

;;;
;; Boot!

(user/init)

(defun user/init ()
  (setq-default auto-save-file-name-transforms        `((".*" ,temporary-file-directory t))
                backup-directory-alist                `((".*" . ,temporary-file-directory))
                fill-column                           120
                frame-resize-pixelwise                t
                indent-tabs-mode                      nil
                inhibit-startup-screen                t
                line-spacing                          2
                vc-follow-symlinks                    t
                whitespace-style                      '(trailing tabs empty indentation))

  (set-default 'truncate-lines t)

  (add-to-list 'load-path "~/.emacs.d/elisp")
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(font . "Hack-10" ))

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-linum-mode 1)
  (show-paren-mode 1)

  (set-face-attribute 'default t :font "Hack-12")

  (global-set-key (kbd "C-, . f")          'open-config-file)

  (global-set-key (kbd "C-c j")            'join-line)
  (global-set-key (kbd "C-c n a")          'increment-number-at-point)
  (global-set-key (kbd "C-c n x")          'decrement-number-at-point)
  (global-set-key (kbd "C-c q")            'fill-region)
  (global-set-key (kbd "C-c t")            'pop-tag)
  (global-set-key (kbd "C-c y e")          'yas-expand)
  (global-set-key (kbd "M-<down>")         'move-text-line-down)
  (global-set-key (kbd "M-<up>")           'move-text-line-up)
  (global-set-key (kbd "C-.")              'er/expand-region)
  (global-set-key (kbd "C-, c")            'comment-region)

  (define-key emacs-lisp-mode-map (kbd "C-, e") 'eval-region)

  ;;;
  ;; Package

  (require 'package)

  (defvar package-list)
  (setq package-list '(
                       ag
                       buffer-move
                       company
                       expand-region
                       fill-column-indicator
                       flycheck
                       golden-ratio
                       helm
                       helm-ag
                       helm-projectile
                       iedit
                       inf-ruby
                       magit
                       nyan-mode
                       projectile
                       spaceline
                       spacemacs-theme
                       magit-gh-pulls
                       markdown-mode
                       rainbow-delimiters
                       rspec-mode
                       rubocop
                       smart-mode-line
                       smartparens
                       yaml-mode
                       yasnippet
                       build-status
                       coffee-mode
                       ))

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
  ;;; coffee-mode
  (require 'coffee-mode)

  ;;;;
  ;;; buffer-move
  (require 'buffer-move)

  ;;;;
  ;;; expand-region
  (require 'expand-region)

  ;;;;
  ;;; build-status
  (setq-default build-status-check-interval 60)
  (require 'build-status)
  (put 'build-status-mode-line-string 'risky-local-variable t)

  (defun show-build-status ()
    (async-start (lambda ()
                   (if (not (eq (build-status--project (buffer-file-name)) nil))
                     (build-status-mode)))))
  (add-hook 'prog-mode-hook 'show-build-status)

  ;;;;
  ;; align
  ;;; Originals from http://d.hatena.ne.jp/rubikitch/20080227/1204051280
  ;;; Via https://github.com/jimweirich/emacs-setup-esk/blob/master/ruby-align.el
  (defun add-custom-align-mappings ()
    (add-to-list 'align-rules-list
                 '(ruby-comma-delimiter
                   (regexp . ",\\(\\s-*\\)[^# \t\n]")
                   (repeat . t)
                   (modes  . '(ruby-mode))))
    (add-to-list 'align-rules-list
                 '(ruby-hash-literal
                   (regexp . "\\(\\s-*\\)=>\\s-*[^# \t\n]")
                   (group 2 3)
                   (repeat . t)
                   (modes  . '(ruby-mode))))
    (add-to-list 'align-rules-list
                 '(ruby-hash-literal2
                   (regexp . "[a-z0-9]:\\(\\s-*\\)[^# \t\n]")
                   (repeat . t)
                   (modes  . '(ruby-mode))))
    (add-to-list 'align-rules-list
                 '(ruby-assignment-literal
                   (regexp . "\\(\\s-*\\)=\\s-*[^# \t\n]")
                   (repeat . t)
                   (modes  . '(ruby-mode))))
    (add-to-list 'align-rules-list
                 '(ruby-xmpfilter-mark
                   (regexp . "\\(\\s-*\\)# => [^#\t\n]")
                   (repeat . nil)
                   (modes . '(ruby-mode))))
    (add-to-list 'align-rules-list
                 '(ruby-brace-block-start
                   (regexp . "\\(\\s-*\\){")
                   (repeat . nil)
                   (modes . '(ruby-mode)))))
  (add-hook 'align-load-hook 'add-custom-align-mappings)
  (global-set-key (kbd "C-, a") 'align)

  ;;;;
  ;;; fill-column-indicator
  (add-hook 'prog-mode-hook 'fci-mode)

  ;;;;
  ;;; rainbow-delimiters
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

  ;;;;
  ;;; rubocop
  (add-hook 'ruby-mode-hook 'rubocop-mode)

  ;;;;
  ;;; Flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (require 'flycheck)
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)

  ;;;;
  ;;; Nyan
  (nyan-mode)

  ;;;;
  ;;; Spaceline
  (setq-default powerline-default-separator 'utf-8)
  (require 'spaceline)
  (require 'spaceline-config)
  (spaceline-emacs-theme)
  (spaceline-toggle-nyan-cat-on)


  ;;;;
  ;;; Whitespace
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;;;;
  ;;; ruby-mode
  (require 'smartparens-ruby)

  ;;;;
  ;;; Theme
  (load-theme 'spacemacs-dark t)

  ;;;;
  ;;; Windmove
  (windmove-default-keybindings)

  ;;;;
  ;;; Helm
  (setq-default helm-mode-fuzzy-match t)
  (require 'helm)
  (require 'helm-ag)
  (require 'helm-ring)
  (helm-mode)
  (global-set-key (kbd "M-x") 'helm-M-x)
  (global-set-key (kbd "M-y") 'helm-show-kill-ring)
  (global-set-key (kbd "C-x C-f") 'helm-find-files)
  (define-key global-map [remap list-buffers] 'helm-mini)
  (define-key global-map [remap find-tag]              'helm-etags-select)
  (define-key global-map [remap xref-find-definitions] 'helm-etags-select)
  (define-key helm-find-files-map "\t" 'helm-execute-persistent-action)
  (add-to-list 'display-buffer-alist
               `(,(rx bos "*helm" (* not-newline) "*" eos)
                 (display-buffer-in-side-window)
                 (inhibit-same-window . t)
                 (window-height . 0.4)))

  ;;;;
  ;;; Golden Ratio
  ;; (golden-ratio-mode) ;; this still needs some tuning

  ;;;;
  ;;; Smartparens
  (smartparens-global-strict-mode t)

  ;;;;
  ;;; magit
  (require 'magit)
  (require 'magit-gh-pulls)
  (global-set-key (kbd "M-g M-s") 'magit)
  (add-hook 'git-commit-mode-hook #'(lambda () (setq fill-column 72)))
  (add-hook 'git-commit-mode-hook 'fci-mode)
  (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
  (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
  (remove-hook 'magit-status-sections-hook 'magit-insert-stashes)
  (add-hook 'magit-gh-pulls-mode-hook (lambda ()
                                        (remove-hook 'magit-status-sections-hook 'magit-gh-pulls-insert-gh-pulls)))

  ;;;;
  ;;; projectile
  (setq-default projectile-completion-system 'helm)
  (require 'projectile)
  (projectile-global-mode)
  (helm-projectile-on)
  (global-set-key (kbd "M-p") 'projectile-find-file)
  (global-set-key (kbd "M-t") 'projectile-find-tag)

  ;;;;
  ;;; company-mode
  (add-hook 'after-init-hook 'global-company-mode)

  ;;;;
  ;;; rspec-mode
  (require 'rspec-mode)
  (add-hook 'after-init-hook 'inf-ruby-switch-setup))

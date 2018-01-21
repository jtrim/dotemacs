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
    (buffer-move rubocop flycheck nyan-mode spaceline helm-ag yaml-mode company magit rspec-mode inf-ruby phoenix-dark-pink-theme molokai-theme inkpot-theme smart-mode-line powerline yasnippet helm helm-ebdb golden-ratio))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "src/")))
(load "helper.el")

;; ==================
;; User Configuration

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

  (setq-default )

  (set-default 'truncate-lines t)

  (add-to-list 'load-path "~/.emacs.d/elisp")
  (add-to-list 'default-frame-alist '(fullscreen . maximized))
  (add-to-list 'default-frame-alist '(font . "Hack-12" ))

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-linum-mode 1)
  (show-paren-mode 1)

  (set-face-attribute 'default t :font "Hack-10")

  (global-set-key (kbd "C-c j")            'join-line)
  (global-set-key (kbd "C-c n a")          'increment-number-at-point)
  (global-set-key (kbd "C-c n x")          'decrement-number-at-point)
  (global-set-key (kbd "C-c q")            'fill-region)
  (global-set-key (kbd "C-c t")            'pop-tag)
  (global-set-key (kbd "C-c y e")          'yas-expand)
  (global-set-key (kbd "M-<down>")         'move-text-line-down)
  (global-set-key (kbd "M-<up>")           'move-text-line-up)
  ;; (global-set-key (kbd "M-m a m") 'helm-imenu)
  ;; (global-set-key (kbd "C-.") 'er/expand-region)

  ;;;
  ;; Package

  (require 'package)

  (defvar package-list)
  (setq package-list '(buffer-move
                       company
                       fill-column-indicator
                       flycheck
                       golden-ratio
                       helm
                       helm-ag
                       helm-projectile
                       inf-ruby
                       magit
                       nyan-mode
                       phoenix-dark-pink-theme
                       projectile
                       rainbow-delimiters
                       rspec-mode
                       rubocop
                       smart-mode-line
                       smartparens
                       spaceline
                       yaml-mode
                       yasnippet
                       ;; eyebrowse
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
  ;;; buffer-move
  (require 'buffer-move)

  ;;;;
  ;;; fill-column-indicator
  (require 'fill-column-indicator)
  (add-hook 'prog-mode-hook 'fci-mode)

  ;;;;
  ;;; rainbow-delimiters
  (require 'rainbow-delimiters)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
  (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)

  ;;;;
  ;;; rubocop
  (require 'rubocop)
  (add-hook 'ruby-mode-hook 'rubocop-mode)

  ;;;;
  ;;; Flycheck
  (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
  (require 'flycheck)
  (add-hook 'prog-mode-hook 'flycheck-mode)
  (add-hook 'text-mode-hook 'flyspell-mode)

  ;;;;
  ;;; Nyan
  (require 'nyan-mode)
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
  (add-hook 'git-commit-mode-hook #'(lambda () (setq fill-column 72)))
  (add-hook 'git-commit-mode-hook 'fci-mode)

  ;;;;
  ;;; projectile
  (setq-default projectile-completion-system 'helm)
  (require 'projectile)
  (projectile-global-mode)

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
;; Boot!

(user/init)

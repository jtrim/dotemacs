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

  ;; Rebalance windows on split
  (defadvice split-window-right (after rebalance-windows activate)
    (balance-windows))
  (defadvice delete-window (after rebalance-windows activate)
    (balance-windows))


  (set-face-attribute 'default t :font "Hack-12")

  (global-set-key (kbd "C-, . f")  'open-config-file)
  (global-set-key (kbd "C-c j")    'join-line)
  (global-set-key (kbd "C-c n a")  'increment-number-at-point)
  (global-set-key (kbd "C-c n x")  'decrement-number-at-point)
  (global-set-key (kbd "C-c q")    'fill-region)
  (global-set-key (kbd "C-c t")    'pop-tag)
  (global-set-key (kbd "C-c y e")  'yas-expand)
  (global-set-key (kbd "M-<down>") 'move-text-line-down)
  (global-set-key (kbd "M-<up>")   'move-text-line-up)
  (global-set-key (kbd "C-.")      'er/expand-region)
  (global-set-key (kbd "C-, c")    'comment-or-uncomment-region)
  (global-set-key (kbd "C--")      'text-scale-decrease)
  (global-set-key (kbd "C-+")      'text-scale-increase)

  (define-key emacs-lisp-mode-map (kbd "C-, e") 'eval-region)

  ;;; Built-in packages

  ;;;;
  ;;; align
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
  ;;; Whitespace
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;;;
  ;; External Package
  (install-and-configure-packages
   '(
     ;; Emacs nicities
     (windmove
      (lambda ()
        (windmove-default-keybindings)))
     buffer-move
     define-word
     (spaceline
      (lambda ()
        (setq-default powerline-default-separator 'utf-8
                      spaceline-workspace-numbers-unicode t)
        (require 'spaceline)
        (require 'spaceline-config)
        (apply 'spaceline--theme
               '((workspace-number
                  buffer-modified
                  buffer-size)
                 :priority 0)
               '((buffer-id remote-host)
                 :priority 5))))
     (eyebrowse
      (lambda ()
        (require 'eyebrowse)
        (eyebrowse-mode t)
        (define-key eyebrowse-mode-map (kbd "ESC M-SPC") 'eyebrowse-next-window-config)
        (define-key eyebrowse-mode-map (kbd "ESC M-DEL") 'eyebrowse-prev-window-config)))

     ;; Editing nicities
     expand-region
     (fill-column-indicator
      (lambda ()
        (add-hook 'prog-mode-hook 'fci-mode)))
     iedit
     (rainbow-delimiters
      (lambda ()
        (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
        (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))
     (smartparens
      (lambda ()
        (smartparens-global-strict-mode t)))

     ;; Searching
     ag

     ;; Helm / projectile
     (helm
      (lambda ()
        (setq-default helm-mode-fuzzy-match t)
        (require 'helm)
        (require 'helm-ring)
        (helm-mode)
        (global-set-key (kbd "M-x") 'helm-M-x)
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
        (global-set-key (kbd "M-y") 'helm-show-kill-ring)))
     (helm-ag
      (lambda ()
        (global-set-key (kbd "C-, r s") 'resume-last-search-buffer)))
     helm-projectile
     (projectile
      (lambda ()
        (setq-default projectile-completion-system 'helm)
        (require 'projectile)
        (projectile-global-mode)
        (helm-projectile-on)
        (global-set-key (kbd "M-p") 'projectile-find-file)
        (global-set-key (kbd "M-t") 'projectile-find-tag)
        (global-set-key (kbd "C-, s") 'projectile-ag)))

     ;; Completion and linting
     (company
      (lambda ()
        (add-hook 'after-init-hook 'global-company-mode)
        (add-hook 'compilation-shell-minor-mode-hook (lambda () (company-mode nil)))))
     (flycheck
      (lambda ()
        (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
        (require 'flycheck)
        (add-hook 'prog-mode-hook 'flycheck-mode)
        (add-hook 'text-mode-hook 'flyspell-mode)))
     yasnippet

     ;; Ruby
     inf-ruby
     (ruby-mode
      (lambda ()
        (require 'smartparens-ruby)))
     (rspec-mode
      (lambda ()
        (require 'rspec-mode)
        (add-hook 'rspec-mode-hook 'inf-ruby-switch-setup)))
     (rubocop
      (lambda ()
        (add-hook 'ruby-mode-hook 'rubocop-mode)))

     ;; Git
     (magit
      (lambda ()
        (require 'magit)
        (global-set-key (kbd "M-g M-s") 'magit)
        (add-hook 'git-commit-mode-hook #'(lambda () (setq fill-column 72)))
        (add-hook 'git-commit-mode-hook 'fci-mode)))
     (magit-gh-pulls
      (lambda ()
        (require 'magit-gh-pulls)
        (add-hook 'magit-mode-hook 'turn-on-magit-gh-pulls)
        (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
        (remove-hook 'magit-status-sections-hook 'magit-insert-stashes)
        (add-hook 'magit-gh-pulls-mode-hook (lambda ()
                                              (remove-hook 'magit-status-sections-hook 'magit-gh-pulls-insert-gh-pulls)))
        (add-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream t)))

     ;; Misc
     (nyan-mode
      (lambda ()
        (nyan-mode)))

     ;; Themes
     (spacemacs-theme
      (lambda ()
        (load-theme 'spacemacs-dark t)))

     ;; Syntax modes
     markdown-mode
     yaml-mode
     slim-mode
     (coffee-mode
      (lambda ()
        (custom-set-variables '(coffee-tab-width 2))
        (require 'coffee-mode)))
     )))

(defun user/init ()
  (setq-default auto-save-file-name-transforms        `((".*" ,temporary-file-directory t))
                backup-directory-alist                `((".*" . ,temporary-file-directory))
                fill-column                           120
                frame-resize-pixelwise                t
                indent-tabs-mode                      nil
                inhibit-startup-screen                t
                line-spacing                          2
                vc-follow-symlinks                    t
                whitespace-style                      '(trailing tabs empty indentation)
                split-height-threshold                nil
                split-width-threshold                 0)

  (set-default 'truncate-lines t)

  (add-to-list 'load-path "~/.emacs.d/elisp")
  (add-to-list 'default-frame-alist '(fullscreen . maximized))

  (tool-bar-mode -1)
  (scroll-bar-mode -1)
  (global-linum-mode 1)
  (show-paren-mode 1)
  (desktop-save-mode -1)
  (defalias 'yes-or-no-p 'y-or-n-p)
  (setq tags-revert-without-query 1)

  ;; Rebalance windows on split
  (defadvice split-window-right (after rebalance-windows activate)
    (balance-windows))
  (defadvice delete-window (after rebalance-windows activate)
    (balance-windows))
  (defadvice split-window-below (after rebalance-windows activate)
    (balance-windows))
  (defadvice xref-goto-xref (after close-xref-buffer activate)
    (kill-xref-window-and-buffer))
  (defadvice delete-other-windows (before snapshot-window-config activate)
    (window-configuration-to-register 1))

  (setq default-global-font-spec (font-spec
                                  :family "Fira Code"
                                  :size   14))
  (set-face-attribute 'default nil :font default-global-font-spec)

  ;; Disable auto-indent for text-mode
  (add-hook 'text-mode-hook 'set-noop-indent-line-function)

  ;; Scratch buffer configuration
  (setq initial-major-mode 'markdown-mode)
  (setq initial-scratch-message "# Scratch

")

  ;; Leader mappings
  (global-set-key (kbd "C-, . f")  'open-config-file)
  (global-set-key (kbd "C-, o")    'open-org-file)
  (global-set-key (kbd "C-, c")    'comment-or-uncomment-region)
  (global-set-key (kbd "C-, t")    'projectile-regen-etags)
  (global-set-key (kbd "C-, z")    'focus-mode)
  (define-key emacs-lisp-mode-map (kbd "C-, e") 'eval-region)

  ;; C-c mappings
  (global-set-key (kbd "C-c j")    'join-line)
  (global-set-key (kbd "C-c n a")  'increment-number-at-point)
  (global-set-key (kbd "C-c n x")  'decrement-number-at-point)
  (global-set-key (kbd "C-c q")    'fill-region)
  (global-set-key (kbd "C-c t")    'pop-tag)
  (global-set-key (kbd "C-c y e")  'yas-expand)

  ;; Unset key
  (global-unset-key (kbd "s-p")) ;; crashes emacs.
  (global-unset-key (kbd "s-w")) ;; is stupid.
  (global-unset-key (kbd "s-q")) ;; is unnecessary.
  (global-unset-key (kbd "<f11>"))
  (global-unset-key (kbd "<f12>"))

  ;; Various mappings
  (global-set-key (kbd "M-<down>") 'move-text-line-down)
  (global-set-key (kbd "M-<up>")   'move-text-line-up)
  (global-set-key (kbd "C-.")      'er/expand-region)
  (global-set-key (kbd "C--")      'decrease-global-font-size)
  (global-set-key (kbd "C-+")      'increase-global-font-size)
  (global-set-key (kbd "C-c <backspace>") (lambda () (interactive) (jump-to-register 1)))
  (global-set-key (kbd "C-c SPC") (lambda () (interactive) (jump-to-register 2)))
  (global-set-key (kbd "<f11> <f11>") (lambda () (interactive) (window-configuration-to-register 1)))
  (global-set-key (kbd "<f12> <f12>") (lambda () (interactive) (window-configuration-to-register 2)))
  (global-set-key (kbd "C-, w") 'kill-ring-save-sexp-forward)
  (global-set-key (kbd "C-, y") 'sp-copy-inside-parent-sexp)
  (global-set-key (kbd "C-<tab>") 'hs-toggle-hiding)

  ;; Misc other things
  (add-hook 'compilation-finish-functions 'bury-compile-buffer-if-successful)

  ;;; Built-in packages

  ;;;;
  ;;; vc
  ;; Turn off vc, prefer magit
  (setq vc-handled-backends nil)

  ;;;;
  ;;; electric indent
  (setq auto-indent-newline-function 'newline-and-indent)

  ;;;;
  ;;; Org mode
  ;; (setq org-replace-disputed-keys  t
  ;;       org-log-done               'time)

  ;;;;
  ;;; imenu
  (require 'imenu)
  (setq imenu-max-item-length 300)
  (global-set-key (kbd "M-i") 'helm-imenu)

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
                   (regexp . "[a-z0-9]:\\(\\s-+\\)[^# \t\n]")
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
  (global-set-key (kbd "C-, a a") 'align-current)
  (global-set-key (kbd "C-, a =") '(lambda ()
                                     (interactive)
                                     (align-regexp (region-beginning) (region-end) "\\(\\s-*\\)=")))

  ;;;;
  ;;; Whitespace
  (add-hook 'prog-mode-hook 'whitespace-mode)
  (add-hook 'before-save-hook 'whitespace-cleanup)

  ;;;;
  ;;; Tramp
  ;;; This require is necessary to allow helm project find to work on a fresh boot of emacsclient into dired. *shrug*
  ;; (require 'tramp)

  ;;;;
  ;;; Windmove
  (require 'windmove)
  (windmove-default-keybindings)

  ;;;;
  ;;; HideShow
  (require 'hideshow)
  (add-to-list 'hs-special-modes-alist
               '(ruby-mode
                 "\\(def\\|do\\|{\\)" "\\(end\\|end\\|}\\)" "#"
                 nil nil))

  ;;;
  ;; External Package
  (install-and-configure-packages
   '(
     ;; Emacs nicities
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

     ;; Editing nicities
     iedit
     (rainbow-delimiters
      (lambda ()
        (require 'rainbow-delimiters)
        (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)
        (add-hook 'prog-mode-hook 'rainbow-delimiters-mode)))
     (smartparens
      (lambda ()
        (smartparens-global-strict-mode t)
        (global-set-key (kbd "C-M-g") 'sp-forward-slurp-sexp)))
     (which-key
      (lambda ()
        (require 'which-key)
        (which-key-mode)))

     ;; Searching
     ag

     ;; Helm / projectile
     (helm
      (lambda ()
        (setq-default helm-mode-fuzzy-match t
                      helm-candidate-number-limit 2000)
        (require 'helm)
        (require 'helm-ring)
        (helm-mode)
        (global-set-key (kbd "M-x") 'helm-M-x)
        (global-set-key (kbd "C-x C-f") 'helm-find-files)
        (define-key global-map [remap list-buffers] 'helm-mini)
        (define-key helm-find-files-map "\t" 'helm-execute-persistent-action)
        (add-to-list 'display-buffer-alist
                     `(,(rx bos "*helm" (* not-newline) "*" eos)
                       (display-buffer-in-side-window)
                       (inhibit-same-window . t)
                       (window-height . 0.4)))
        (add-hook 'helm-major-mode-hook
                  (lambda ()
                    (setq auto-composition-mode nil)))
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
     (flyspell
      (lambda ()
        (require 'flyspell)
        (define-key flyspell-mode-map (kbd "C-,") nil)))
     (flycheck
      (lambda ()
        (setq-default flycheck-disabled-checkers '(emacs-lisp-checkdoc))
        (require 'flycheck)
        (add-hook 'prog-mode-hook 'flycheck-mode)
        (add-hook 'text-mode-hook 'flyspell-mode)))

     ;; Ruby
     (inf-ruby
      (lambda ()
        (require 'inf-ruby)
        (add-hook 'compilation-filter-hook 'inf-ruby-auto-enter)
        (define-key inf-ruby-minor-mode-map
          (kbd "C-c C-s") 'inf-ruby-console-auto)))
     (ruby-mode
      (lambda ()
        (require 'smartparens-ruby)
        (require 'ruby-mode)
        (define-key ruby-mode-map (kbd "C-, b") 'ruby-change-to-multi-line-keyword-args)
        (add-hook 'ruby-mode-hook (lambda () (hs-minor-mode)))))
     (rspec-mode
      (lambda ()
        (setq-default rspec-autosave-buffer nil)
        (require 'rspec-mode)
        (add-hook 'rspec-compilation-mode-hook 'inf-ruby-switch-setup)
        (add-hook 'rspec-compilation-mode-hook (lambda () (setq auto-composition-mode nil)))
        (add-hook 'dired-mode-hook 'rspec-dired-mode)))
     (rubocop
      (lambda ()
        (add-hook 'ruby-mode-hook 'rubocop-mode)))
     chruby
     yari
     (robe
      (lambda ()
        (require 'robe)
        (add-hook 'ruby-mode-hook 'robe-mode)
        (define-key robe-mode-map (kbd "M-.") nil)))

     ;; Git
     (magit
      (lambda ()
        (setq-default magit-save-repository-buffers nil)
        (setq magit-refresh-status-buffer nil)
        (require 'magit)
        (global-set-key (kbd "M-g M-s") 'magit)
        (add-hook 'git-commit-mode-hook #'(lambda () (setq fill-column 72)))
        (remove-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream-or-recent)
        (remove-hook 'magit-status-sections-hook 'magit-insert-stashes)
        (add-hook 'magit-status-sections-hook 'magit-insert-unpushed-to-upstream t)
        (magit-define-popup-action 'magit-commit-popup
          ?W "WIP Commit" '(lambda ()
                             (interactive)
                             (magit-run-git "add" ".")
                             (magit-run-git "commit" "-am" "WIP [ci skip]" "--no-verify" "--no-gpg-sign")))
        ;; Speed up commit window by turning off the automatic diff
        (remove-hook 'server-switch-hook 'magit-commit-diff)))
     (magithub
      (lambda ()
        (require 'magithub)
        (magithub-feature-autoinject t)))

     ;; Themes
     (spacemacs-theme
      (lambda ()
        (load-theme 'spacemacs-dark t)))

     ;; Syntax modes
     (markdown-mode
      (lambda()
        (require 'markdown-mode)
        (add-hook 'markdown-mode-hook 'turn-off-smartparens-strict-mode)))
     yaml-mode
     slim-mode
     (coffee-mode
      (lambda ()
        (custom-set-variables '(coffee-tab-width 2))
        (require 'coffee-mode)))
     (yasnippet
      (lambda ()
        (require 'yasnippet)
        ;;(add-to-list yas-snippet-dirs "~/.emacs.d/snippets")
        (yas-global-mode 1)))
     (slack
      (lambda ()
        (require 'slack)))

     ;; (xterm-color
     ;;  (lambda ()
     ;;    (setq comint-output-filter-functions
     ;;          (remove 'ansi-color-process-output comint-output-filter-functions))

     ;;    (add-hook 'shell-mode-hook
     ;;              (lambda () (add-hook 'comint-preoutput-filter-functions 'xterm-color-filter nil t)))

     ;;    ;; Also set TERM accordingly (xterm-256color)
     ;;    ;; You can also use it with eshell (and thus get color output from system ls):
     ;;    (require 'eshell)

     ;;    (add-hook 'eshell-before-prompt-hook
     ;;              (lambda () (setq xterm-color-preserve-properties t)))

     ;;    (add-to-list 'eshell-preoutput-filter-functions 'xterm-color-filter)
     ;;    (setq eshell-output-filter-functions (remove 'eshell-handle-ansi-color eshell-output-filter-functions))))
     ))

  ;;;;
  ;;; magit-rspec
  ;;; depends on both magit and rspec
  (require 'magit-rspec)
  (define-key rspec-mode-map (kbd "C-c , g") 'magit-rspec-run-changed-files))

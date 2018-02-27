(package-initialize)

;;;
;; Custom
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("a27c00821ccfd5a78b01e4f35dc056706dd9ede09a8b90c6955ae6a390eb1c1e" "84e39ed4c552b75e1cb09458c140a9b025598002533456b4c27db31d27e1e0d7" "b571f92c9bfaf4a28cb64ae4b4cdbda95241cd62cf07d942be44dc8f46c491f4" "c74e83f8aa4c78a121b52146eadb792c9facc5b1f02c917e3dbb454fca931223" "b9e9ba5aeedcc5ba8be99f1cc9301f6679912910ff92fdf7980929c2fc83ab4d" "84d2f9eeb3f82d619ca4bfffe5f157282f4779732f48a5ac1484d94d5ff5b279" "3c83b3676d796422704082049fc38b6966bcad960f896669dfc21a7a37a748fa" default)))
 '(package-selected-packages
   (quote
    (lua-mode slim-mode anzu winum chruby eyebrowse coffee-mode helm-projectile build-status magit-gh-pulls expand-region markdown-mode define-word thingatpt+ spacemacs-theme ir-black-theme ag iedit buffer-move rubocop flycheck nyan-mode spaceline helm-ag yaml-mode company magit rspec-mode inf-ruby phoenix-dark-pink-theme molokai-theme inkpot-theme smart-mode-line powerline yasnippet helm helm-ebdb golden-ratio))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

(add-to-list 'load-path (concat user-emacs-directory (convert-standard-filename "src/")))
(load "helper.el")

;;;
;; Boot!

(load (concat user-emacs-directory "src/config.el"))
(load (concat user-emacs-directory "src/package-configuration.el"))
(user/init)
(require 'server)
(unless (server-running-p) (server-start))
(put 'narrow-to-region 'disabled nil)

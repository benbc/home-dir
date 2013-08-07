(add-hook 'before-save-hook 'delete-trailing-whitespace)
(global-auto-revert-mode 1)
(setq auto-revert-check-vc-info t)
(set-face-attribute 'default nil :height 120)
(global-unset-key (kbd "C-x C-z"))

(eval-after-load "grep"
  '(progn
    (add-to-list 'grep-find-ignored-files "TAGS")
    (add-to-list 'grep-find-ignored-directories ".bundle")
    (add-to-list 'grep-find-ignored-directories "log")))

(require 'package)

(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/") t)
(package-initialize)

(when (not package-archive-contents)
  (package-refresh-contents))

(defvar my-packages '(graphviz-dot-mode
                      starter-kit
                      starter-kit-bindings
                      starter-kit-ruby
                      starter-kit-lisp
                      clojure-mode
                      color-theme)
  "A list of packages to ensure are installed at launch.")

(dolist (p my-packages)
  (when (not (package-installed-p p))
    (package-install p)))

(require 'color-theme)
(color-theme-arjen)

(setq rcirc-default-nick "benbc")
(setq rcirc-default-user-name "benbc")
(setq rcirc-default-user-full-name "Ben Butler-Cole")
(setq rcirc-server-alist
      '(("irc.freenode.org" :channels ())))
(setq rcirc-authinfo
      '(("freenode" nickserv "benbc" "<%= @irc_password %>")))
(rcirc-track-minor-mode 1)
(rcirc nil)

(setq js-indent-level 2)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(safe-local-variable-values (quote ((eval sh-set-shell "bash") (eval sh-set-shell (quote bash)) (whitespace-line-column . 80) (lexical-binding . t)))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

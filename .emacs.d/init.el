(defvar bootstrap-version)
(let ((bootstrap-file
       (expand-file-name "straight/repos/straight.el/bootstrap.el" user-emacs-directory))
      (bootstrap-version 5))
  (unless (file-exists-p bootstrap-file)
    (with-current-buffer
        (url-retrieve-synchronously
         "https://raw.githubusercontent.com/raxod502/straight.el/develop/install.el"
         'silent 'inhibit-cookies)
      (goto-char (point-max))
      (eval-print-last-sexp)))
  (load bootstrap-file nil 'nomessage))

 
;;; Packages
 
(straight-use-package 'use-package)
(setq straight-use-package-by-default t) ; always use :straight t
(setq use-package-always-demand (daemonp)) ; if emacs is running in a daemon, load all packages immediately

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  :config
  (evil-mode 1))

(use-package evil-collection
  :after evil)

;; We don't call (projectile-mode 1) because counsel-projectile takes care of that
(use-package projectile
  :config
  (setq projectile-project-search-path '("~/code/")))

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t))

(use-package counsel
  :config
  (counsel-mode 1))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))

(use-package company
  :config
  (company-mode 1)
  (setq company-dabbrev-downcase 0)
  (setq company-idle-delay 0))

(use-package typescript-mode
	:config
	(setq-default typescript-indent-level 2))

(use-package protobuf-mode)

(setq-default js-indent-level 2)

(use-package lsp-mode
  :hook ((js-mode	  ; ts-ls (theia-ide)
	  js-jsx-mode 	  ; ts-ls (theia-ide)
	  typescript-mode ; ts-ls (theia-ide)
	  ) . lsp)
  :config
  (setq lsp-keymap-prefix "SPC l"))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-show-with-mouse nil)
  (setq lsp-ui-doc-show-with-cursor nil))
(use-package lsp-ivy)

(use-package magit)


(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-vibrant t))

(use-package which-key
  :config
  (which-key-mode))

(use-package general)

(evil-collection-init) ; init evil collection keybindings after all other packages load
;;; Keybindings

;; One less keypress than :
(general-swap-key nil 'motion
  ";" ":")

(general-define-key
 ;; Window Motion
 "M-h" 'evil-window-left
 "M-j" 'evil-window-down
 "M-k" 'evil-window-up
 "M-l" 'evil-window-right

 "C-x C-e" 'eval-region)

(general-create-definer leader-def
  :states '(normal visual) 
  :keymaps 'override
  :prefix "SPC"
  :non-normal-prefix "S-SPC")

(leader-def
  "." 'find-file
  "b" 'buffer-menu
  "p" 'projectile-command-map)


;;; Config

;; (set-frame-font ...) in init.el doesn't work for emacsclient
;;   since it needs to be run for every frame
(add-to-list 'default-frame-alist '(font . "Iosevka SS02-12"))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(setq mouse-autoselect-window t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default display-line-numbers 'relative)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory ".emacs-backups"))))



;;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(warning-suppress-types '((comp) (comp) (comp) (comp) (comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

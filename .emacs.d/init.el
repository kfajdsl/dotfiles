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

(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode +1)
  (setq evil-snipe-scope 'whole-visible))

(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

;; We don't call (projectile-mode 1) because counsel-projectile takes care of that
(use-package projectile
  :config
  (setq projectile-project-search-path '("~/code/")))

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy))))

(use-package counsel
  :config
  (counsel-mode 1))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))

(use-package company
  :config
  (global-company-mode)
  (setq company-dabbrev-downcase nil)
  (setq company-idle-delay 0))

(use-package pdf-tools
  :config
  (pdf-tools-install)
  :general
  (:states 'normal
           ";" 'evil-ex))

(use-package typescript-mode
	:config
	(setq-default typescript-indent-level 2))

(use-package protobuf-mode)

(setq-default js-indent-level 2)

(use-package flycheck
  :config
  (global-flycheck-mode))

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

(use-package all-the-icons)

(use-package treemacs
  :config
  (treemacs-resize-icons 20))
(use-package treemacs-evil)
(use-package treemacs-projectile)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-one-light t)
  (setq doom-themes-treemacs-theme "doom-colors")
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  (doom-themes-treemacs-config))

(use-package which-key
  :config
  (which-key-mode))

(use-package general)

(use-package mini-modeline
  :config
  (mini-modeline-mode t))

(use-package vterm)

(use-package adaptive-wrap
  :config
  (adaptive-wrap-prefix-mode t))



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

(defun turn-off-line-numbers ()
  (setq-local display-line-numbers nil))

(defun add-to-multiple-hooks (func hooks)
  (mapc (lambda (hook)
          (add-hook hook func))
        hooks))
(add-to-multiple-hooks
 'turn-off-line-numbers
 '(vterm-mode-hook help-mode-hook magit-mode-hook treemacs-mode-hook pdf-view-mode-hook))


(setq backup-directory-alist `(("." . ,(concat user-emacs-directory ".emacs-backups"))))
(treemacs-create-theme "kaolin"
      :config
      (progn
        ;; Set fallback icon
        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-octicon "file-text"
                                                    :height 1.10
                                                    :v-adjust 0.1))
         :extensions (fallback))

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-material "subject"
                                                   :v-adjust -0.2
                                                   :height 1.3
                                                   :face 'font-lock-variable-name-face))
         :extensions (root))

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-material "folder_open"
                                               ;; :v-adjust 0.05
                                               :height 1.1))
         ;; :face 'font-lock-doc-face))
         :extensions (dir-open))

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-material "folder"
                                                   ;; :v-adjust 0.05
                                                   :height 1.1))

         :extensions (dir-closed))

        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-material "close"
                                             :size 1.0
                                             ;; :v-adjust 0.1
                                             :face 'font-lock-keyword-face))
         :extensions (tag-open))

        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-faicon "chevron-down"
                                             :size 0.9
                                             :v-adjust 0.1
                                             :face 'font-lock-keyword-face))
         :extensions (tag-closed))

        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-faicon "tag"
                                             :height 0.9
                                             :face 'font-lock-type-face))
         :extensions (tag-leaf))

        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-material "error"
                                             :height 0.9
                                             :face 'error))
                              :extensions (error)
                              :fallback (propertize "• " 'face 'font-lock-warning-face))
        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-material "warning"
                                                     :height 0.9
                                                     :face 'error))
         :extensions (warning)
         :fallback (propertize "• " 'face 'font-lock-string-face))
        (treemacs-create-icon
         :icon (format "%s " (all-the-icons-material "info"
                                                     :height 0.9
                                                     :face 'font-lock-string-face))
         :extensions (info)
         :fallback (propertize "• " 'face 'font-lock-string-face))

        ;; Icons for filetypes
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "csharp-line"))
                              :extensions ("cs"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "css3")) :extensions ("css"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "git")) :extensions ("gitignore" "git" "gitattributes" "gitconfig" "gitmodules"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "html5")) :extensions ("html" "htm"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "java")) :extensions ("java" "jar"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "python")) :extensions ("py"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "rust")) :extensions ("rs"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "haskell")) :extensions ("hs"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "c")) :extensions ("c" "h"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "crystal")) :extensions ("cr"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "cplusplus")) :extensions ("cpp" "cxx" "hpp" "tpp" "cc" "hh"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "ruby-alt")) :extensions ("rb"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "scala")) :extensions ("scala"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "elixir")) :extensions ("ex" "exs"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "erlang")) :extensions ("erl" "hrl"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "clojure")) :extensions ("clj" "cljs" "cljc"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "cabal")) :extensions ("cabal"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "clisp")) :extensions ("lisp"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "go")) :extensions ("go"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "graphql")) :extensions ("graphql" "gql"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "graphviz")) :extensions ("dot" "gv"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "elisp" :v-adjust -0.15)) :extensions ("el" "elc"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "julia")) :extensions ("jl"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "kotlin")) :extensions ("kt" "kts"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "hy")) :extensions ("hy"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "javascript-badge")) :extensions ("js"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "jsx2-alt")) :extensions ("jsx"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "ocaml")) :extensions ("ml" "mli"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "org")) :extensions ("org"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "php")) :extensions ("php"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "terminal")) :extensions ("sh" "zsh" "fish"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "typescript")) :extensions ("ts" "tsx"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "npm")) :extensions ("package.json" "package-lock.json"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "nimrod")) :extensions ("nim" "nims"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "perl")) :extensions ("pl" "plx" "pm" "perl"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "perl6")) :extensions ("pm6" "p6" "t6" "raku" "rakumod" "rakudoc" "rakutest"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "R")) :extensions ("r"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "tex")) :extensions ("tex"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "rst")) :extensions ("rst"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "vue")) :extensions ("vue"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "markdown" :v-adjust 0.05)) :extensions ("md" "markdown"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "file-pdf")) :extensions ("pdf"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-octicon "database")) :extensions ("sql"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-material "style")) :extensions ("styles"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "lua")) :extensions ("lua"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "asciidoc")) :extensions ("adoc" "asciidoc"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "sbt")) :extensions ("sbt"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "puppet")) :extensions ("pp"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "jinja")) :extensions ("j2" "jinja2"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "dockerfile")) :extensions ("dockerfile"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "vagrant")) :extensions ("vagrantfile"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "racket")) :extensions ("racket" "rkt" "rktl" "rktd" "scrbl" "scribble" "plt"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "reason")) :extensions ("re" "rei"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "sass")) :extensions ("scss" "sass"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "stylus")) :extensions ("styl"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-alltheicon "less")) :extensions ("less"))
        (treemacs-create-icon :icon (format " %s " (all-the-icons-material "style")) :extensions ("styles"))


        ;; Media files icon
        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-octicon "file-media" :v-adjust 0.1))
         :extensions ("jpg" "jpeg" "png" "gif" "ico" "tif" "tiff" "svg" "bmp"
                      "psd" "ai" "eps" "indd" "mov" "avi" "mp4" "webm" "webp"
                      "mkv" "wav" "mp3" "ogg" "midi"))

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-octicon "file-text"
                       :height 1.1
                       :v-adjust 0.05))
         :extensions ("rst" "log" "txt" "contribute" "license" "readme" "changelog"))

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-faicon "cogs"))
         :extensions ("conf" "cfg" "yaml" "yml" "json" "xml" "toml" "cson" "ini" "iml"))

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-octicon "code"))
         :extensions ("a.out" "tpl" "erb" "mustache" "twig" "ejs" "mk" "haml" "pug" "jade"))

        (treemacs-create-icon
         :icon (format " %s " (all-the-icons-octicon "file-zip"))
         :extensions ("zip" "xz" "tar" "gz" "7z" "rar"))
        ))

;;; Customize

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(electric-pair-mode t)
 '(warning-suppress-types '((comp) (comp) (comp) (comp) (comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:height 1.0 :family "Noto Sans")))))

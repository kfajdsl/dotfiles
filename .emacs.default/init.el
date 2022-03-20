;;; init.el --- Personal emacs config
;;; Commentary:
;; bruh

;;; Code:


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


(use-package exec-path-from-shell
  :config
  (dolist (var '("SSH_AUTH_SOCK" "SSH_AGENT_PID"
                 "GPG_AGENT_INFO" "LANG"
                 "LC_CTYPE" "NIX_SSL_CERT_FILE"
                 "NIX_PATH"))
    (add-to-list 'exec-path-from-shell-variables var))
  (when (memq window-system '(mac ns x pgtk))
    (exec-path-from-shell-initialize))
  (when (daemonp)
    (exec-path-from-shell-initialize)))

(use-package evil
  :init
  (setq evil-want-integration t)
  (setq evil-want-keybinding nil)
  (setq evil-split-window-below t)
  (setq evil-vsplit-window-right t)
  (setq evil-ex-substitute-global t)
  (setq evil-ex-search-case 'sensitive)
  :config
  (evil-mode 1)
  (evil-set-undo-system 'undo-redo))

(use-package evil-snipe
  :after evil
  :config
  (evil-snipe-mode +1)
  (setq evil-snipe-scope 'whole-visible))


(use-package evil-collection
  :after evil
  :config
  (evil-collection-init))

(use-package general)

;; We don't call (projectile-mode 1) because counsel-projectile takes care of that
(use-package projectile
  :config
  (setq projectile-project-search-path '("~/dev/" "~/ros2-ws/src/"))
  (setq projectile-globally-ignored-directories '("~/.platformio/")))

(use-package ivy
  :config
  (ivy-mode 1)
  (setq ivy-re-builders-alist
      '((t . ivy--regex-fuzzy))))

(use-package counsel
  :config
  (counsel-mode 1))

(use-package counsel-projectile
  :config
  (counsel-projectile-mode 1))

(use-package perspective
  :init
  (setq persp-state-default-file (concat user-emacs-directory ".persp-save-state"))
  :config
  (persp-mode)
  (add-hook 'kill-emacs-hook #'persp-state-save))

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

(use-package nix-mode
  :config
  (add-hook 'nix-mode-hook 'lsp))

(use-package elixir-mode
  :config
  (add-hook 'elixir-mode-hook 'lsp))
(use-package inf-elixir)

(use-package direnv
  :config
  (direnv-mode))

(use-package pipenv
  :hook (python-mode . pipenv-mode)
  :init
  (setq
   pipenv-projectile-after-switch-function
   #'pipenv-projectile-after-switch-extended))

(use-package hcl-mode)


(use-package smart-tabs-mode)

(use-package lsp-mode
  :hook ((js-mode	  ; ts-ls (theia-ide)
	  js-jsx-mode 	  ; ts-ls (theia-ide)
	  typescript-mode ; ts-ls (theia-ide)
	  ) . lsp)
  :config
  (setq gc-cons-threshold 100000000)
  (setq read-process-output-max (* 1024 1024))
  (setq lsp-keymap-prefix "SPC l")
  (setq lsp-lens-enable nil)
  (load-file (concat user-emacs-directory "straight/repos/lsp-mode/lsp-icons.el")))

(use-package lsp-ui
  :config
  (setq lsp-ui-doc-show-with-cursor nil))
;(use-package lsp-ivy)
;
;(use-package lsp-python-ms
;  :init
;  (setq lsp-python-ms-auto-install-server (executable-find "python-language-server")))

(use-package magit)

(use-package docker)

(use-package all-the-icons)

(use-package treemacs
  :config
  (treemacs-resize-icons 20)
  (setq treemacs-read-string-input 'from-minibuffer)
  (add-hook 'projectile-switch-project #'treemacs-display-current-project-exclusively)
  (add-hook 'persp-switch-hook #'treemacs-display-current-project-exclusively))
(use-package treemacs-evil)
(use-package treemacs-projectile)

(use-package doom-themes
  :config
  (setq doom-themes-enable-bold t    ; if nil, bold is universally disabled
        doom-themes-enable-italic t) ; if nil, italics is universally disabled
  (load-theme 'doom-acario-dark t)
  (setq doom-themes-treemacs-theme "doom-colors")
  (setq doom-themes-treemacs-enable-variable-pitch nil)
  (doom-themes-treemacs-config))

(use-package which-key
  :config
  (which-key-mode))


;(use-package mini-modeline
;  :config
;  (mini-modeline-mode t))

(use-package vterm)

(defun run-in-vterm-kill (process event)
  "A process sentinel. Kills PROCESS's buffer if it is live."
  (let ((b (process-buffer process)))
    (and (buffer-live-p b)
         (kill-buffer b))))

(defun run-in-vterm (command)
  "Execute string COMMAND in a new vterm.

Interactively, prompt for COMMAND with the current buffer's file
name supplied. When called from Dired, supply the name of the
file at point.

Like `async-shell-command`, but run in a vterm for full terminal features.

The new vterm buffer is named in the form `*foo bar.baz*`, the
command and its arguments in earmuffs.

When the command terminates, the shell remains open, but when the
shell exits, the buffer is killed."
  (interactive
   (list
    (let* ((f (cond (buffer-file-name)
                    ((eq major-mode 'dired-mode)
                     (dired-get-filename nil t))))
           (filename (concat " " (shell-quote-argument (and f (file-relative-name f))))))
      (read-shell-command "Terminal command: "
                          (cons filename 0)
                          (cons 'shell-command-history 1)
                          (list filename)))))
  (with-current-buffer (vterm (concat "*" command "*"))
    (set-process-sentinel vterm--process #'run-in-vterm-kill)
    (vterm-send-string command)
    (vterm-send-return)))

(use-package adaptive-wrap
  :config
  (adaptive-wrap-prefix-mode t))

(use-package minions
  :config (minions-mode 1))

(use-package persp-projectile)

(use-package evil-surround
  :config
  (global-evil-surround-mode 1))

(use-package web-mode
  :init
  (setq web-mode-markup-indent-offset 2)
  (setq web-mode-code-indent-offset 2)
  (setq web-mode-enable-auto-pairing t)
  (setq web-mode-enable-auto-closing t)
  :config
  (add-to-list 'auto-mode-alist '("\\.phtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.tpl\\.php\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.[agj]sp\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.as[cp]x\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.erb\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.mustache\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.djhtml\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html?\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\.leex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.html\\.eex\\'" . web-mode))
  (add-to-list 'auto-mode-alist '("\\.sface\\'" . web-mode)))
(define-derived-mode my-svelte-mode web-mode "Svelte")
(add-to-list 'auto-mode-alist '("\\.svelte\\'" . my-svelte-mode))
(add-hook 'my-svelte-mode-hook 'lsp)
(setq css-indent-offset 2)

(use-package ripgrep)
(use-package projectile-ripgrep)

(use-package ligature
  :straight (ligature :type git :host github :repo "mickeynp/ligature.el")
  :config
  ;; Enable all Iosevka ligatures in programming modes
  (ligature-set-ligatures 'prog-mode '("<---" "<--"  "<<-" "<-" "->" "-->" "--->"
                                       "<->" "<-->" "<--->" "<---->" "<!--" "<=="
                                       "<===" "<=" "=>" "=>>" "==>" "===>" ">="
                                       "<=>" "<==>" "<===>" "<====>" "<!---" "<~~"
                                       "<~" "~>" "~~>" "::" ":::" "==" "!=" "==="
                                       "!==" ":=" ":-" ":+" "<*" "<*>" "*>" "<|"
                                       "<|>" "|>" "+:" "-:" "=:" "<******>" "++" "+++"))
  (global-ligature-mode 1))

(use-package terraform-mode)

(use-package dockerfile-mode)

(use-package lsp-pyright
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp)))
  :init
  (setq lsp-disabled-clients '(mspyls-remote))
  )  ; or lsp-deferred(lsp-register-client

(lsp-register-client
    (make-lsp-client :new-connection (lsp-tramp-connection (cons "pyright-langserver" "--stdio"))
                     :major-modes '(python-mode)
                     :remote? t
                     :server-id 'pyls-remote))

(use-package docker-compose-mode)

(use-package ccls
  :hook ((c-mode c++-mode objc-mode cuda-mode) .
         (lambda () (require 'ccls) (lsp)))
  :config
  (setq ccls-executable "~/src/ccls/Release/ccls"))


(use-package platformio-mode)

(use-package lsp-java
  :config
  (add-hook 'java-mode-hook #'lsp))

(use-package tree-sitter)
(use-package tree-sitter-langs)

(use-package grip-mode
  :config
  (setq grip-preview-use-webkit t)
  (setq grip-update-after-change nil))


(show-paren-mode 1)

;;; Keybindings

;; One less keypress than :
(general-define-key
 :states 'motion
 :keymaps 'override
 ";" 'evil-ex)


(general-define-key
 ;; Window Motion
 "M-h" 'evil-window-left
 "M-j" 'evil-window-down
 "M-k" 'evil-window-up
 "M-l" 'evil-window-right

 "C-x C-e" 'eval-region)

(general-define-key
 :keymaps 'vterm-mode-map
 "M-h" 'evil-window-left
 "M-j" 'evil-window-down
 "M-k" 'evil-window-up
 "M-l" 'evil-window-right)

(defun override-evil-collection-eshell ()
  "Override eshell evil-collection keybinds."
  (general-define-key
   :states '(motion normal insert visual)
   :keymaps 'eshell-mode-map
   ;; Why evil collection? Normal mode exists
   "M-h" 'evil-window-left
   "M-l" 'evil-window-right))
(add-hook 'eshell-mode-hook 'override-evil-collection-eshell)

(defun override-treemacs-evil ()
  "Override treemacs-evil keybinds."
  (evil-define-key 'treemacs treemacs-mode-map (kbd "M-l") 'evil-window-right))
(add-hook 'treemacs-mode-hook 'override-treemacs-evil)





(general-create-definer leader-def
  ; treemacs is special
  :states '(normal visual treemacs)
  :keymaps '(override evil-treemacs-state-map)
  :prefix "SPC"
  :non-normal-prefix "S-SPC")

(leader-def
  "." 'find-file
  "b" 'persp-counsel-switch-buffer
  "i" 'persp-ibuffer
  "t" 'treemacs
  "g" 'magit
  "p r" 'projectile-ripgrep
  "p s" 'projectile-switch-project
  "p f" 'projectile-find-file)

;;; Config

;; (set-frame-font ...) in init.el doesn't work for emacsclient
;;   since it needs to be run for every frame
(add-to-list 'default-frame-alist '(font . "Iosevka-13:bold"))
(add-to-list 'default-frame-alist '(vertical-scroll-bars . nil))
(add-to-list 'default-frame-alist '(undecorated . t))
(setq frame-resize-pixelwise t)

(menu-bar-mode -1)
(toggle-scroll-bar -1)
(tool-bar-mode -1)

(setq mouse-autoselect-window t)

(setq-default indent-tabs-mode nil)
(setq-default tab-width 2)
(setq-default display-line-numbers nil)
(setq ring-bell-function 'ignore)

(setq backup-directory-alist
      `((".*" . "~/.saves")))
(setq auto-save-file-name-transforms
      `((".*" "~/.saves" t)))
(setq create-lockfiles nil)

(defun add-to-multiple-hooks (func hooks)
  (mapc (lambda (hook)
          (add-hook hook func))
        hooks))

(defun turn-on-line-numbers ()
  (setq-local display-line-numbers 'relative))
(add-to-multiple-hooks
 'turn-on-line-numbers
 '(emacs-lisp-mode-hook js-mode-hook typescript-mode-hook elixir-mode-hook shell-mode-hook lsp-mode-hook elixir-mode-hook prog-mode-hook))

(defun my-c-style ()
  (setq c-basic-offset 4)
  (setq tab-width 4))
(add-to-multiple-hooks
 'my-c-style
 '(c-mode-hook c++-mode-hook))
(setq c-default-style '((java-mode . "java")
                       (awk-mode . "awk")
                       (other . "java")))

(defun load-only-theme (theme &optional no-confirm no-enable) ; this fucked with me for too long
  "Disable all themes then load a single theme."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar #'symbol-name
				     (custom-available-themes))))
    nil nil))
  (unless (custom-theme-name-valid-p theme)
    (error "Invalid theme name `%s'" theme))
  ;; If THEME is already enabled, re-enable it after loading, even if
  ;; NO-ENABLE is t.
  (if no-enable
      (setq no-enable (not (custom-theme-enabled-p theme))))
  ;; If reloading, clear out the old theme settings.
  (when (custom-theme-p theme)
    (disable-theme theme)
    (put theme 'theme-settings nil)
    (put theme 'theme-feature nil)
    (put theme 'theme-documentation nil))
  (let ((file (locate-file (concat (symbol-name theme) "-theme.el")
                           (custom-theme--load-path)
                           '("" "c")))
        (custom--inhibit-theme-enable t))
    ;; Check file safety with `custom-safe-themes', prompting the
    ;; user if necessary.
    (cond ((not file)
           (error "Unable to find theme file for `%s'" theme))
          ((or no-confirm
               (eq custom-safe-themes t)
               (and (memq 'default custom-safe-themes)
                    (equal (file-name-directory file)
                           (expand-file-name "themes/" data-directory))))
           ;; Theme is safe; load byte-compiled version if available.
           (load (file-name-sans-extension file) nil t nil t))
          ((with-temp-buffer
             (insert-file-contents file)
             (let ((hash (secure-hash 'sha256 (current-buffer))))
               (when (or (member hash custom-safe-themes)
                         (custom-theme-load-confirm hash))
                 (eval-buffer nil nil file)
                 t))))
          (t
           (error "Unable to load theme `%s'" theme))))
  ;; Optimization: if the theme changes the `default' face, put that
  ;; entry first.  This avoids some `frame-set-background-mode' rigmarole
  ;; by assigning the new background immediately.
  (let* ((settings (get theme 'theme-settings))
         (tail settings)
         found)
    (while (and tail (not found))
      (and (eq (nth 0 (car tail)) 'theme-face)
           (eq (nth 1 (car tail)) 'default)
           (setq found (car tail)))
      (setq tail (cdr tail)))
    (when found
      (put theme 'theme-settings (cons found (delq found settings)))))
  ;; DISABLE OTHER THEMES ;; WHY WOULD ANYONE WANT TO DO THIS
  (while custom-enabled-themes
    (disable-theme (car custom-enabled-themes)))
  ;; Finally, enable the theme.
  (unless no-enable
    (enable-theme theme))
  t)

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
        ;(treemacs-create-icon :icon (format " %s " (all-the-icons-fileicon "perl6")) :extensions ("pm6" "p6" "t6" "raku" "rakumod" "rakudoc" "rakutest"))
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
 '(custom-safe-themes
   '("f91395598d4cb3e2ae6a2db8527ceb83fed79dbaf007f435de3e91e5bda485fb" "7eea50883f10e5c6ad6f81e153c640b3a288cd8dc1d26e4696f7d40f754cc703" "8d7b028e7b7843ae00498f68fad28f3c6258eda0650fe7e17bfb017d51d0e2a2" "cbdf8c2e1b2b5c15b34ddb5063f1b21514c7169ff20e081d39cf57ffee89bc1e" "7a7b1d475b42c1a0b61f3b1d1225dd249ffa1abb1b7f726aec59ac7ca3bf4dae" "da186cce19b5aed3f6a2316845583dbee76aea9255ea0da857d1c058ff003546" "f6665ce2f7f56c5ed5d91ed5e7f6acb66ce44d0ef4acfaa3a42c7cfe9e9a9013" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "6b1abd26f3e38be1823bd151a96117b288062c6cde5253823539c6926c3bb178" "6036bcd187c6ff160ee394684d25931fd2413ad78889ca80a06c7dc65a948748" "a41d7d4c20bfa90be5450905a69f65a8ae35d3bcb97f11dfaef47036cf72a372" "46911ffe468b3547bb8d973f364830779374a33010510cd6e2a36b53b27ac256" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" default))
 '(electric-pair-mode t)
 '(warning-suppress-types '((comp) (comp) (comp) (comp) (comp) (comp) (comp))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(variable-pitch ((t (:height 1.0 :family "Noto Sans")))))

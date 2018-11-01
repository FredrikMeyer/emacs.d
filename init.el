;;;;
;; Packages
;;;;

(server-start)

(setq user-full-name "Fredrik Meyer"
      user-mail-address "hrmeyer@gmail.com")

(setq ns-pop-up-frames nil)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; No need for ~ files when editing
(setq create-lockfiles nil)


(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

(setq inhibit-startup-message t
      initial-scratch-message nil
      )

;; Define package repositories
(require 'package)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.

(package-initialize)

(setq package-enable-at-startup nil)
(setq tls-checktrust 't)
(add-to-list 'package-archives
             '("marmalade" . "https//marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))


(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)


;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
;; (when (not package-archive-contents)
;;   (package-refresh-contents))


;; The packages you want installed. You can also install these
;; manually with M-x package-install
;; Add in your own as you wish:
(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))

(use-package try
  :ensure t)

(use-package which-key
             :ensure t
             :config
             (which-key-mode))

    ;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
(use-package paredit
    :ensure t
    :config
    (add-hook 'prog-mode-hook 'paredit-everywhere-mode))

    ;; key bindings and code colorization for Clojure
    ;; https://github.com/clojure-emacs/clojure-mode
(use-package clojure-mode
  :ensure t)

    ;; extra syntax highlighting for clojure
(use-package clojure-mode-extra-font-locking
  :ensure t)

    ;; integration with a Clojure REPL
    ;; https://github.com/clojure-emacs/cider
(use-package cider 
  :ensure t)

;; http://www.emacswiki.org/emacs/InteractivelyDoThings
(use-package ido
  :ensure t
  :init
  (progn (ido-mode t)
         (ido-ubiquitous-mode 1))
  :config
  ;; This allows partial matches, e.g. "tl" will match "Tyrion Lannister"
  (setq ido-enable-flex-matching t)
  ;; Includes buffer names of recently open files, even if they're not open now
  (setq ido-use-virtual-buffers t)
  ;; Turn this behavior off because it's annoying
  (setq ido-use-filename-at-point nil)
  ;; Don't try to match file across all "work" directories; only match files
  ;; in the current directory displayed in the minibuffer
  ;;(setq ido-auto-merge-work-directories-length -1)
  (progn
    ;; Define he following variables to remove the compile-log warnings
    ;; when defining ido-ubiquitous
    (defvar ido-cur-item nil)
    (defvar ido-default-item nil)
    (defvar ido-cur-list nil)
    (defvar predicate nil)
    (defvar inherit-input-method nil))
  )

   ;; allow ido usage in as many contexts as possible. see
    ;; customizations/navigation.el line 23 for a description
    ;; of ido
(use-package ido-completing-read+ 
  :ensure t)

    ;; Enhances M-x to allow easier execution of commands. Provides
    ;; a filterable list of possible commands in the minibuffer
    ;; http://www.emacswiki.org/emacs/Smex
(use-package smex
  :ensure t)

(use-package projectile
  :ensure t)

(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "C-x t") 'neotree-toggle)
  :config
  (progn
    (setq neo-smart-open t)
    (setq neo-show-hidden-files t))
  )


(use-package rainbow-delimiters
  :ensure t)

(use-package geiser
  :ensure t)

(use-package ac-geiser
  :ensure t)

(use-package pretty-lambdada
  :ensure t)

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status))

(use-package org
  :ensure t
  :config
  (progn
    (add-hook 'org-mode-hook 'visual-line-mode)
    (add-hook 'org-mode-hook 'auto-save-mode)
    )
  )

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

(use-package js2-mode
  :ensure t
  :config
  (setq-default indent-tabs-mode nil)
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode)
  :config
  (add-hook 'js-mode-hook 'electric-pair-mode)
  :config
  (add-hook 'js-mode-hook 'electric-indent-mode)
  )

(use-package company
  :ensure t
;  :config comment until company works for elm
  ;; (add-to-list 'company-backends 'company-elm)
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0.1)
  )

(use-package elm-mode
  :ensure t
  :config
  (setq-default indent-tabs-mode nil)
  :config
  (add-hook 'elm-mode-hook 'electric-pair-mode)
  :config
  (add-hook 'elm-mode-hook 'electric-indent-mode)
;  :config
                                        ;  (add-hook 'elm-mode-hook 'turn-off-elm-indent)
  :config
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  :config
  (add-hook 'elm-mode-hook 'company-mode)
  :config
  (setq elm-format-on-save t)
  :config
  (setq elm-interactive-command '("elm" "repl")
        elm-reactor-command '("elm" "reactor")
        elm-reactor-arguments '("--port" "8000")
        elm-compile-command '("elm" "make")
        elm-compile-arguments '("--output=elm.js" "--debug")
        elm-package-command '("elm" "package")
        elm-package-json "elm.json")
  )

(use-package slime
  :ensure t
  :init
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  :config
  (setq inferior-lisp-program "/usr/local/bin/ccl")
  :config
  (setq slime-contribs '(slime-fancy slime-repl))
  )

(use-package ruby-mode
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'ruby-electric-mode))

(use-package ruby-electric
  :ensure t
)

(use-package seeing-is-believing
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'seeing-is-believing)
)

(use-package inf-ruby
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'haskell-mode))

(use-package go-mode
  :ensure t
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  :config
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  :config
  (add-hook 'go-mode-hook 'electric-pair-mode)
  :config
  (add-hook 'go-mode-hook 'electric-indent-mode)
  :config
  (add-hook 'go-mode-hook (lambda ()
            (setq indent-tabs-mode 1)
            (setq tab-width 4)))
)


(use-package markdown-mode
  :ensure t
  :config
  (progn (add-hook 'markdown-mode-hook 'visual-line-mode)
         (add-hook 'markdown-mode-hook  (lambda ()
                                          (let ((file (file-name-nondirectory buffer-file-name)))
                                            (format "pandoc -o %s.pdf %s --pdf-engine=xelatex"
                                                    (file-name-sans-extension file)
                                                    file)))
                                          )
                                          
   )  
  :mode ("\\.md$"))


(add-hook 'doc-view-mode-hook 'auto-revert-mode)

(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package solarized-theme
  :ensure t
  :config
  (load-theme 'solarized-light t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9"))
  )

(use-package minions
  :ensure t
  :config (minions-mode 1)
  )

(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1)
  )

(use-package yasnippet-snippets
  :ensure t)


;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell

(if (eq system-type 'darwin)
    (use-package exec-path-from-shell
      :ensure t)
)



;; Shows a list of buffers
(defalias 'list-buffers  'ibuffer)


;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))


(rainbow-delimiters-mode t)

;;(setq electric-indent-mode nil)

;;;;
;; Customization
;;;;


;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; These customizations make it easier for you to navigate files,
;; switch buffers, and choose options from the minibuffer.
(load "navigation.el")

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; Hard-to-categorize customizations
(load "misc.el")

;; For editing lisps
(load "elisp-editing.el")

;; Language-specific
(load "setup-clojure.el")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline bold bold-italic bold])
 '(coffee-tab-width 2)
 '(custom-safe-themes
   (quote
    ("aaffceb9b0f539b6ad6becb8e96a04f2140c8faa1de8039a343a4f1e009174fb" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default)))
 '(debug-on-error nil)
 '(minions-mode t)
 '(package-selected-packages
   (quote
    (minions dracula-theme solarized-theme neotree go-mode haskell-mode ruby-electric inf-ruby elm-mode try which-key use-package htmlize restclient yasnippet-snippets json-mode sml-mode markdown-mode tagedit smex rainbow-delimiters projectile paredit magit ido-ubiquitous exec-path-from-shell clojure-mode-extra-font-locking cider))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )


;; Removes tool-bar
;; (if (fboundp 'scroll-bar-mode) (scroll-bar-mode -1))
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))
;(if (fboundp 'menu-bar-mode) (menu-bar-mode 1))


;; javascript mode



;(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
;(add-hook 'js-mode-hook 'js2-minor-mode)
;(add-hook 'js2-mode-hook 'ac-js2-mode)
;(add-hook 'js-mode-hook (lambda () (tern-mode t)))


;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
;;(require 'auto-complete-config)
;(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
;(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
;(ac-set-trigger-key "TAB")
;(ac-set-trigger-key "<tab>")




;;; org-mode

(setq org-src-fontify-natively t
    org-src-tab-acts-natively t
   org-confirm-babel-evaluate nil
   org-edit-src-content-indentation 0)



;;; racket

(require 'ac-geiser)
(add-hook 'geiser-mode-hook 'ac-geiser-setup)
(add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
(eval-after-load "auto-complete"
  '(add-to-list 'ac-modes 'geiser-repl-mode))

(setq geiser-active-implementations '(chicken racket))
(show-paren-mode 1)

(add-to-list 'pretty-lambda-auto-modes 'geiser-repl-mode)
(pretty-lambda-for-modes)

(dolist (mode pretty-lambda-auto-modes)
  ;; add paredit-mode to all mode-hooks
  (add-hook (intern (concat (symbol-name mode) "-hook")) 'paredit-mode))

;(add-hook 'racket-repl-mode-hook #'rainbow-delimiters-mode)
;(add-hook 'racket-repl-mode-hook #'auto-complete-mode)

;(add-hook 'racket-mode (setq tab-always-indent 'complete))
;(setq tab-always-indent 'complete)


;;;;;;;;;; Oz

(or (getenv "OZHOME")
    (setenv "OZHOME" 
            "/Applications/Mozart2.app/Contents/Resources/"))   ; or wherever Mozart is installed
(setenv "PATH" (concat (getenv "OZHOME") "/bin:" (getenv "PATH")))
 
(setq load-path (cons (concat (getenv "OZHOME") "/share/mozart/elisp")
                      load-path))
 
(setq auto-mode-alist 
      (append '(("\\.oz\\'" . oz-mode)
                ("\\.ozg\\'" . oz-gump-mode))
              auto-mode-alist))
 
(autoload 'run-oz "oz" "" t)
(autoload 'oz-mode "oz" "" t)
(autoload 'oz-gump-mode "oz" "" t)
(autoload 'oz-new-buffer "oz" "" t)
(add-hook 'oz-mode-hook 'electric-pair-mode 'electric-indent-mode)

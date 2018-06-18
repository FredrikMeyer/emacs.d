;;;;
;; Packages
;;;;

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      x-select-enable-clipboard t)

(setq inhibit-startup-message t
      initial-scratch-message nil
      initial-major-mode 'org-mode)

;; Define package repositories
(require 'package)
(setq package-enable-at-startup nil)
(add-to-list 'package-archives
             '("marmalade" . "http://marmalade-repo.org/packages/"))
(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/"))
(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))
(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)


;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.

(package-initialize)

;; Download the ELPA archive description if needed.
;; This informs Emacs about the latest versions of all packages, and
;; makes them available for download.
;; (when (not package-archive-contents)
;;   (package-refresh-contents))

;; Define he following variables to remove the compile-log warnings
;; when defining ido-ubiquitous
(defvar ido-cur-item nil)
(defvar ido-default-item nil)
(defvar ido-cur-list nil)
(defvar predicate nil)
(defvar inherit-input-method nil)

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
    :ensure t)

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

(use-package rainbow-delimiters
  :ensure t)

(use-package geiser
  :ensure t)

(use-package ac-geiser
  :ensure t)

(use-package pretty-lambdada
  :ensure t)

(use-package magit
  :ensure t)

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

;; On OS X, an Emacs instance started from the graphical user
;; interface will have a different environment than a shell in a
;; terminal window, because OS X does not run a shell during the
;; login. Obviously this will lead to unexpected results when
;; calling external utilities like make from Emacs.
;; This library works around this problem by copying important
;; environment variables from the user's shell.
;; https://github.com/purcell/exec-path-from-shell
;(if (eq system-type 'darwin)
 ;   (add-to-list 'my-packages 'exec-path-from-shbell))

;;;;

;; (dolist (p my-packages)
;;   (when (not (package-installed-p p))
;;     (package-install p)))


;; Place downloaded elisp files in ~/.emacs.d/vendor. You'll then be able
;; to load them.
;;
;; For example, if you download yaml-mode.el to ~/.emacs.d/vendor,
;; then you can add the following code to this file:
;;
;; (require 'yaml-mode)
;; (add-to-list 'auto-mode-alist '("\\.yml$" . yaml-mode))
;; 
;; Adding this code will make Emacs enter yaml mode whenever you open
;; a .yml file
(add-to-list 'load-path "~/.emacs.d/vendor")


;;;;
;; Customization
;;;;

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")

;; Sets up exec-path-from-shell so that Emacs will use the correct
;; environment variables
(load "shell-integration.el")

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

;; Langauage-specific
(load "setup-clojure.el")
;;(load "setup-js.el")
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(coffee-tab-width 2)
 '(package-selected-packages
   (quote
    (try which-key use-package htmlize restclient yasnippet-snippets json-mode sml-mode markdown-mode tagedit smex rainbow-delimiters projectile paredit magit ido-ubiquitous exec-path-from-shell clojure-mode-extra-font-locking cider))))
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

(require 'auto-complete)
; do default config for auto-complete
(require 'auto-complete-config)
(ac-config-default)
;; start yasnippet with emacs

(require 'yasnippet)
(yas-global-mode 1)


;(add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
;(add-to-list 'interpreter-mode-alist '("node" . js2-mode))
;(add-hook 'js-mode-hook 'js2-minor-mode)
;(add-hook 'js2-mode-hook 'ac-js2-mode)
;(add-hook 'js-mode-hook (lambda () (tern-mode t)))


;;; auto complete mod
;;; should be loaded after yasnippet so that they can work together
(require 'auto-complete-config)
(add-to-list 'ac-dictionary-directories "~/.emacs.d/ac-dict")
(ac-config-default)
;;; set the trigger key so that it can work together with yasnippet on tab key,
;;; if the word exists in yasnippet, pressing tab will cause yasnippet to
;;; activate, otherwise, auto-complete will
(ac-set-trigger-key "TAB")
(ac-set-trigger-key "<tab>")

(add-hook 'prog-mode-hook 'paredit-everywhere-mode)


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

(setq geiser-active-implementations '(racket))
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

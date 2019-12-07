;;;
;; EMACS config
;;;


;;; Code:

(toggle-debug-on-error t)
(server-start)

(setq lexical-binding t)

(setq user-full-name "Fredrik Meyer"
      user-mail-address "hrmeyer@gmail.com")

(setq ns-pop-up-frames nil)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)

;; Go straight to scratch buffer on startup
(setq inhibit-startup-message t)

;; No need for ~ files when editing
(setq create-lockfiles nil)

(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      select-enable-clipboard t)

;; Removes tool-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq inhibit-startup-message t
      initial-scratch-message nil
      calendar-week-start-day 1
      )

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; EDITING

;; Highlights matching parenthesis
(show-paren-mode 1)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; HippieExpand: M-n for å fullføre noe
;; http://www.emacswiki.org/emacs/HippieExpand
(setq hippie-expand-try-functions-list
      '(try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-expand-whole-kill
        try-complete-file-name
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-n") 'hippie-expand)

;; Define package repositories
(require 'package)
(setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)

(setq package-enable-at-startup nil)
(setq tls-checktrust 't)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/"))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.org/packages/"))

(add-to-list 'package-archives
             '("melpa-stable" . "http://stable.melpa.org/packages/"))

(add-to-list 'package-archives
             '("org" . "https://orgmode.org/elpa/"))


(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)


(unless (package-installed-p 'use-package)
	(package-refresh-contents)
	(package-install 'use-package))


(use-package try
  :ensure t)

(use-package which-key
             :ensure t
             :config
             (which-key-mode))

(use-package expand-region
  :ensure t
  :config
  (global-set-key (kbd "C-=") 'er/expand-region))

(use-package flycheck
  :ensure t
  :config
  (global-flycheck-mode)
  (add-hook 'flycheck-mode-hook 'add-node-modules-path)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode))

(use-package flycheck-flow
  :ensure t)

(defun eslint-fix-file ()
  "Run eslint fix current file."
  (interactive)
  (message "yarn eslinteslint --fixing the file" (buffer-file-name))
  (shell-command (concat "yarn eslint --fix " (buffer-file-name))))

(setq js2-strict-missing-semi-warning nil)

;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
(use-package paredit
    :ensure t
    :config
    (add-hook 'prog-mode-hook 'paredit-everywhere-mode))

(use-package flycheck-clj-kondo
  :ensure t
  :after (flycheck))

;; key bindings and code colorization for Clojure
;; https://github.com/clojure-emacs/clojure-mode
(use-package clojure-mode
  :ensure t
  :config
  (require 'flycheck-clj-kondo)
  ;; Enable paredit for Clojure
  (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  :config
  ;; Java classes (e.g. JavaClassName)
  (add-hook 'clojure-mode-hook 'subword-mode)
  (add-hook 'clojure-mode-hook 'electric-indent-mode)
  (add-hook 'clojure-mode-hook 'electric-pair-mode))

;; extra syntax highlighting for clojure
(use-package clojure-mode-extra-font-locking
  :ensure t)

;; Integration with a Clojure REPL
;; https://github.com/clojure-emacs/cider
(use-package cider
  :ensure t
  :config
  ;; provides minibuffer documentation for the code you're typing into the repl
  (add-hook 'cider-mode-hook 'eldoc-mode)
  :config
  (add-hook 'cider-repl-mode-hook #'company-mode)
  :config
  (add-hook 'cider-mode-hook #'company-mode))

(use-package clj-refactor
  :ensure t
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (clj-refactor-mode 1)
              (yas-minor-mode 1)
              (cljr-add-keybindings-with-prefix "C-c C-m"))))

(use-package restclient
  :ensure t)

(dotimes (n 10)
  (global-unset-key (kbd (format "C-%d" n)))
  (global-unset-key (kbd (format "M-%d" n)))
  )

;; https://github.com/wasamasa/eyebrowse
(use-package eyebrowse
  :ensure t
  :config (progn
            (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
            (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
            (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
            (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
            (eyebrowse-mode t)
            (setq eyebrowse-post-window-switch-hook 'neo-global--attach)
            (setq eyebrowse-new-workspace t)))

(use-package windmove
  :ensure t
  :config
  (windmove-default-keybindings))

;; Disabled because it does not conform with the newest org mode version
;; (use-package ox-reveal
;;   :ensure t)

(use-package smex
  :ensure t)

(use-package counsel
  :ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)
   ("RET" . ivy-alt-done)
   ("<backspace>" . 'ivy-backward-delete-char)))

(use-package ivy
  :ensure t
  :pin melpa
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy)
  (setq ivy-wrap t)
  ;; Fuzzy matching is the best
  (setq ivy-re-builders-alist
        '((counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (swiper-isearch . ivy--regex-plus)
          (counsel-projectile-find-file . ivy--regex-plus)
          (t . ivy--regex-fuzzy)))
  (setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) ""))

(use-package swiper
  :ensure t
  :bind (("C-s" . swiper-isearch)
	 ("C-r" . swiper-isearch)
	 ("C-c C-r" . ivy-resume)
	 ("M-x" . counsel-M-x)
	 ("C-x C-f" . counsel-find-file))
  :config
  (progn
    (ivy-mode 1)
    (setq ivy-use-virtual-buffers t)
    (setq ivy-display-style 'fancy)
    (define-key read-expression-map (kbd "C-r") 'counsel-expression-history)
    ))

;; https://github.com/asok/all-the-icons-ivy
(use-package all-the-icons-ivy
  :ensure t
  :config
  (all-the-icons-ivy-setup))

(use-package ivy-rich
  :ensure t
  :config
  :disabled
  (ivy-rich-mode 1))

;; https://github.com/hrs/engine-mode
(use-package engine-mode
  :ensure t
  :config (engine-mode t)
  :config
  (defengine github
    "https://github.com/search?ref=simplesearch&q=%s"
    :keybinding "G")
  (defengine google
    "http://www.google.com/search?ie=utf-8&oe=utf-8&q=%s"
    :keybinding "g")
  (defengine wikipedia
    "http://www.wikipedia.org/search-redirect.php?language=en&go=Go&search=%s"
    :keybinding "w"
    :docstring "Searchin' the wikis."))

;; https://github.com/hrehfeld/emacs-smart-hungry-delete
(use-package smart-hungry-delete
  :ensure t
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
		 ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks
  :config (smart-hungry-delete-add-default-hooks))

(use-package anzu
  :ensure t
  :bind (("C-1" . anzu-query-replace)
         ("C-!" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package projectile
  :ensure t
;  :config ;; Use counsel-projectile
;  (projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :config
  (setq projectile-project-compilation-cmd ""))

; https://github.com/ericdanan/counsel-projectile
(use-package counsel-projectile
  :ensure t
  :config
  (counsel-projectile-mode))

(use-package projectile-ripgrep
  :ensure t)

;; https://github.com/dajva/rg.el
(use-package rg
  :ensure t
  :config
  (rg-enable-default-bindings)
  (setq rg-executable "/usr/local/bin/rg"))

(use-package neotree
  :ensure t
  :config
  (global-set-key (kbd "C-x t") 'neotree-toggle)
  :config
  (progn
    (setq neo-smart-open t)
    (setq neo-theme 'icons)
    (setq neo-window-fixed-size nil)
    (setq neo-show-hidden-files t))
  ;; https://github.crookster.org/macOS-Emacs-26-display-line-numbers-and-me/
  (add-hook 'neo-after-create-hook (lambda (&rest _) (display-line-numbers-mode -1))))

;; https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons
  :ensure t)

(use-package all-the-icons-dired
  :ensure t
  :config
  (add-hook 'dired-mode-hook 'all-the-icons-dired-mode))

(use-package rainbow-delimiters
  :ensure t
  :config
  (add-hook 'prog-mode-hook #'rainbow-delimiters-mode))

(use-package geiser
  :ensure t)

(use-package ac-geiser
  :ensure t)

;; https://github.com/TeMPOraL/nyan-mode
(use-package nyan-mode
  :ensure t
  :disabled
  :config
  (setq nyan-wavy-trail 1)
  (nyan-mode))


;; lambda
(use-package pretty-lambdada
  :ensure t)

(use-package undo-tree
  :ensure t
  :disabled
  :init
  (global-undo-tree-mode)
  :config
  (undo-tree))

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x C-g") 'magit-list-repositories)
  (global-set-key (kbd "C-x C-B") 'magit-blame-addition)
  (setq magit-repository-directories
        `(("~/code" . 1)
          ("~/entur" . 1)
          (,user-emacs-directory . 1)))
  (setq magit-list-refs-sortby "-creatordate"))

;; https://github.com/syohex/emacs-git-messenger
(use-package git-messenger
  :ensure t
  :bind ("C-c M" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

;; https://github.com/nonsequitur/git-gutter-plus
(use-package git-gutter+
  :ensure t
  :config
  (global-git-gutter+-mode))

(use-package org
  :ensure t
  :pin org
  :config
  (progn
    (add-hook 'org-mode-hook 'visual-line-mode)
    (add-hook 'org-mode-hook 'auto-save-mode))
  :config
  (setq org-src-fontify-natively t
        org-src-tab-acts-natively t
        org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0)
  :config
  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
                               (calc . t)
                               (clojure . t)
                               (gnuplot . t)))
  (setq org-babel-clojure-backend 'cider)
  :config
  (setq org-default-notes-file "~/Dropbox/org/tasks.org")
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-capture-templates '(("w" "Work todo" entry (file "~/entur/notes.org")
                               "* TODO %?\n%U" :empty-lines 1)
                              ("t" "Todo" entry (file "~/Dropbox/org/tasks.org")
                               "* TODO %?\n%U" :empty-lines 1)))
  (require 'org-tempo)
  (require 'ox-md))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))

;; In order for org mode / gnuplot to work
(use-package gnuplot
  :ensure t)

(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file "~/Dropbox/org/notater.org")))

(global-set-key (kbd "C-c ø")
                (lambda () (interactive) (find-file "~/Dropbox/org/okonomi.org")))

(global-set-key (kbd "C-c i")
                (lambda () (interactive (find-file "~/.emacs.d/init.el"))))


(use-package company
  :ensure t
  :config
  (add-hook 'after-init-hook 'global-company-mode)
  :config
  (setq company-idle-delay 0)
  (setq company-dabbrev-downcase nil)
  (add-to-list 'company-backends 'company-flow))

(use-package company-flow
  :ensure t)

(use-package web-mode
  :ensure t
  :after (add-node-modules-path)
  :config
  (progn
    (setq web-mode-indentation-params '("lineup-calls" . nil))
    (add-to-list 'auto-mode-alist '("\\.jsx?$" . web-mode)) ;; auto-enable for .js/.jsx files
    (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?\\'")))
    (add-to-list 'auto-mode-alist '("\\.tsx\\'" . typescript-mode))
    (add-hook 'web-mode-hook (lambda ()
                               (progn (tern-mode)
                                      (flycheck-mode 1)
                                      (electric-indent-mode t)
                                      (electric-pair-mode t))))
    (setq web-mode-enable-auto-quoting nil))
  )

(use-package tide
  :ensure t
  :after (typescript-mode company flycheck)
  :hook ((typescript-mode . tide-setup)
         (typescript-mode . tide-hl-identifier-mode)
         (before-save . tide-formater-before-save)))

(use-package flow-minor-mode
  :ensure t
  :config
  (add-hook 'web-mode-hook 'flow-minor-enable-automatically))

(use-package add-node-modules-path
  :ensure t)

(use-package js2-mode
  :disabled
  :ensure t
  :config
  (setq-default indent-tabs-mode nil)
  :config
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  :config
  (add-hook 'js-mode-hook 'electric-pair-mode)
  :config
  (add-hook 'js-mode-hook 'electric-indent-mode))

(use-package tern
  :ensure t)

(use-package company-tabnine
  :disabled
  :ensure t
  :config
  (add-to-list 'company-backends #'company-tabnine))

(use-package company-tern
  :ensure t
  :after tern
  :config
  (add-hook 'js2-mode-hook (lambda ()
                             (add-to-list (make-local-variable 'company-backends)
                                  '(company-tern company-dabbrev))))
  :config
  (add-hook 'js2-mode-hook (lambda ()
                             (setq company-idle-delay 0)
                             (tern-mode t)
                             (company-mode))))

(use-package glsl-mode
  :ensure t
  :config
  (add-to-list 'auto-mode-alist '("\\.glsl\\'" . glsl-mode)))

(use-package elm-mode
  :ensure t
  :config
  (setq-default indent-tabs-mode nil)
  :config
  (add-hook 'elm-mode-hook 'electric-pair-mode)
  :config
  (add-hook 'elm-mode-hook 'electric-indent-mode)
  :config
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
;  :config
;  (add-to-list 'company-backends 'company-elm)
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
        elm-package-json "elm.json"))

(use-package elm-yasnippets
  :ensure t)

(use-package slime
  :ensure t
  :init
  (load (expand-file-name "~/quicklisp/slime-helper.el"))
  :config
  (setq inferior-lisp-program "/usr/local/bin/ccl")
  :config
  (setq slime-contribs '(slime-fancy slime-repl))
  )

;; Ruby
(use-package ruby-mode
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'ruby-electric-mode))

(use-package ruby-electric
  :ensure t)

;; https://github.com/JoshCheek/seeing_is_believing
;; https://github.com/jcinnamond/seeing-is-believing
(use-package seeing-is-believing
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'seeing-is-believing))

(use-package inf-ruby
  :ensure t
  :config
  (add-hook 'ruby-mode-hook 'inf-ruby-minor-mode))

;; Haskell
(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'haskell-mode)
  (setq haskell-process-type 'stack-ghci)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   (append '((company-capf company-dabbrev-code))
                           company-backends)))))

(use-package intero
  :ensure t
  :disabled
  :config
  (add-hook 'haskell-mode-hook 'intero-mode))

;; Go
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

;; https://www.racket-mode.com/#racket_002dinsert_002dlambda
(use-package racket-mode
  :ensure t
  :config
  (setq racket-program "/usr/local/bin/racket")
  :config
  (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
  :config
  (add-hook 'racket-mode-hook
            (lambda () (electric-indent-mode t))))

(use-package minimap
  :ensure t
  :config
  (setq minimap-window-location 'right)
  (setq minimap-automatically-delete-window nil)
  :config
  (global-set-key (kbd "C-x w") 'minimap-mode))


(use-package markdown-mode
  :ensure t
  :config
  (progn (add-hook 'markdown-mode-hook 'visual-line-mode)
         (add-hook 'markdown-mode-hook  (lambda ()
                                          (let ((file (file-name-nondirectory buffer-file-name)))
                                            (format "pandoc -o %s.pdf %s --pdf-engine=xelatex"
                                                    (file-name-sans-extension file)
                                                    file)))))
  :mode ("\\.md$")
  :config
  (setq markdown-header-scaling 1)
  :config
  (setq markdown-command
      (concat
       "/usr/local/bin/pandoc"
       " --from=markdown --to=html"
       " --standalone --mathjax --highlight-style=pygments")))


;; Automatically refreshes PDF 
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(add-hook 'doc-view-mode-hook (lambda () (linum-mode 0)))

;; LATEX

(use-package company-auctex
  :ensure t
  :config
  (add-hook 'latex-mode-hook (company-auctex-init))
  :config
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

;; Tabs and ribbons for the mode line
;; https://github.com/tarsius/moody
(use-package moody
  :ensure t
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

;; https://github.com/johanvts/emacs-fireplace
(use-package fireplace
  :ensure t)

;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :ensure t
  :config
  (global-set-key (kbd "M-o") 'ace-window))

(use-package spacemacs-common
  :ensure spacemacs-theme
  :config
  (load-theme 'spacemacs-light t))

(use-package solarized-theme
  :ensure t
  :disabled
  :config
  (load-theme 'solarized-light t)
  (let ((line (face-attribute 'mode-line :underline)))
    (set-face-attribute 'mode-line          nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :overline   line)
    (set-face-attribute 'mode-line-inactive nil :underline  line)
    (set-face-attribute 'mode-line          nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :box        nil)
    (set-face-attribute 'mode-line-inactive nil :background "#f9f2d9")))

;; This package implements a menu that lists enabled minor-modes, as well as commonly but not currently enabled minor-modes.
;; https://github.com/tarsius/minions
(use-package minions
  :ensure t
  :config (minions-mode 1))

;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :ensure t
  :init
  (yas-global-mode 1))

(use-package yasnippet-snippets
  :ensure t)

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

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
      :ensure t))

;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(when (memq window-system '(mac ns))
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

;; https://www.emacswiki.org/emacs/WinnerMode
;; Winner Mode is a global minor mode. When activated, it allows you to “undo” (and “redo”) changes in the window configuration with the key commands ‘C-c left’ and ‘C-c right’
(winner-mode)
(global-set-key (kbd "C-S-c <left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-c <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-c <up>") 'enlarge-window)
(global-set-key (kbd "C-S-c <down>") 'shrink-window)

(use-package dired-subtree
  :ensure t
  :after dired
  :config
  (bind-key "<tab>" 'dired-subtree-toggle dired-mode-map))

;; Shows a list of buffers
(defalias 'list-buffers  'ibuffer)
(setq ibuffer-saved-filter-groups
      (quote (("default"
               ("dired" (mode . dired-mode))
               ("PDF" (mode . pdf-view-mode))
               ("python" (mode . python-mode))
               ("org" (or (mode . org-mode)
                          (mode . org-agenda-mode)
                          ))
               ("web" (mode . web-mode))
               ("emacs" (or
                         (name . "^\\*scratch\\*$")
                         (name . "^\\*Messages\\*$")))))))

(setq ibuffer-formats 
      '((mark modified read-only " "
              (name 30 30 :left :elide) ; change: 30s were originally 18s
              " "
              (size 9 -1 :right)
              " "
              (mode 16 16 :left :elide)
              " " filename-and-process)
        (mark " "
              (name 16 -1)
              " " filename)))



(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; Indent region better:
(global-set-key (kbd "M-i") 'indent-region)


;; https://www.emacswiki.org/emacs/AutoIndentation
(electric-indent-mode 1)

;; Add a directory to our load path so that when you `load` things
;; below, Emacs knows where to look for the corresponding file.
(add-to-list 'load-path "~/.emacs.d/customizations")



;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html

(require 'uniquify)
(setq uniquify-buffer-name-style 'forward)

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(use-package recentf
  :ensure t
  :config
  (progn
    (setq recentf-save-file (concat user-emacs-directory ".recentf"))
    (recentf-mode 1)
    (setq recentf-max-menu-items 200)))



(setq-default cursor-type 'bar)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)


(setq sgml-quick-keys 'close)

;; These customizations change the way emacs looks and disable/enable
;; some user interface elements
(load "ui.el")

;; These customizations make editing a bit nicer.
(load "editing.el")

;; For editing lisps
(load "elisp-editing.el")

;; Language-specific
(load "setup-clojure.el")


(show-paren-mode 1)

;; (add-to-list 'pretty-lambda-auto-modes 'geiser-repl-mode)
(pretty-lambda-for-modes)

(dolist (mode pretty-lambda-auto-modes)
  ;; add paredit-mode to all mode-hooks
  (add-hook (intern (concat (symbol-name mode) "-hook")) 'paredit-mode))


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


;;;; Useful functions 

(defun insert-current-date ()
  "Insert current date."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun die-tabs ()
  "Replace all tabs in buffer with spaces."
  (interactive)
  (set-variable 'tab-width 4)
  (mark-whole-buffer)
  (untabify (region-beginning) (region-end))
  (keyboard-quit))

(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(global-set-key (kbd "C-æ") 'toggle-comment-on-line)

(defun indent-buffer ()
  "Indent buffer."
  (interactive)
  (save-excursion
    (indent-region (point-min) (point-max) nil)))
(global-set-key [f12] 'indent-buffer)


;;; init.el ends here

;;;
;; EMACS config
;;;

(toggle-debug-on-error 1)
(server-start)

(setq lexical-binding t)

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
      calendar-week-start-day 1
      )

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)


;; Define package repositories
(require 'package)

;; Load and activate emacs packages. Do this first so that the
;; packages are loaded before you start trying to modify them.
;; This also sets the load path.

(package-initialize)

(setq package-enable-at-startup nil)
(setq tls-checktrust 't)
(add-to-list 'package-archives
             '("marmalade" . "https://marmalade-repo.org/packages/"))

(add-to-list 'package-archives
             '("tromey" . "http://tromey.com/elpa/"))

(add-to-list 'package-archives
             '("melpa" . "http://melpa.milkbox.net/packages/"))

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


(defun eslint-fix-file ()
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
  (add-hook 'clojure-mode-hook (lambda ()
                                 (clj-refactor-mode 1)
                                 (yas-minor-mode 1)
                                 (cljr-add-keybindings-with-prefix "C-c C-m"))))


(use-package restclient
  :ensure t
  :config)

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

;; Disabled because it does not conform with the newest org mode version
;; (use-package ox-reveal
;;   :ensure t)

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
  :diminish (ivy-mode)
  :bind (("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy)
  ;; Fuzzy matching is the best
  (setq ivy-re-builders-alist
        '((t . ivy--regex-fuzzy))))

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
  :config (smart-hungry-delete-add-default-hooks)
  )

(use-package projectile
  :ensure t
  :config
  (projectile-mode)
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  :config
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  :config
  (setq projectile-project-compilation-cmd ""))

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
    (setq neo-window-fixed-size nil)
    (setq neo-show-hidden-files t))
  ;; https://github.crookster.org/macOS-Emacs-26-display-line-numbers-and-me/
  (add-hook 'neo-after-create-hook (lambda (&rest _) (display-line-numbers-mode -1))))


(use-package rainbow-delimiters
  :ensure t
  :config
  (rainbow-delimiters-mode t))


(use-package geiser
  :ensure t)

(use-package ac-geiser
  :ensure t)

;; https://github.com/TeMPOraL/nyan-mode
(use-package nyan-mode
  :ensure t
  :config
  (setq nyan-wavy-trail 1)
  (nyan-mode))


;; lambda
(use-package pretty-lambdada
  :ensure t)

(use-package magit
  :ensure t
  :config
  (global-set-key (kbd "C-x g") 'magit-status)
  (global-set-key (kbd "C-x C-g") 'magit-list-repositories)
  (setq magit-repository-directories
        `(("~/code" . 1)
          ("~/entur" . 1)
          (,user-emacs-directory . 1)))
  (setq magit-list-refs-sortby "-creatordate"))

;; https://github.com/syohex/emacs-git-messenger
(use-package git-messenger
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
                               (clojure . t)))
  (setq org-babel-clojure-backend 'cider)
  :config
  (setq org-default-notes-file "~/Dropbox/org/tasks.org")
  (global-set-key (kbd "C-c c") 'org-capture)
  (require 'org-tempo))

;; In order for org mode / gnuplot to work
(use-package gnuplot
  :ensure t)

(setq org-capture-templates '(("w" "Work todo" entry (file "~/entur/notes.org")
                               "* TODO %?\n%U" :empty-lines 1)
                              ("t" "Todo" entry (file "~/Dropbox/org/tasks.org")
                               "* TODO %?\n%U" :empty-lines 1)))

(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file "~/Dropbox/org/notater.org")))

(global-set-key (kbd "C-c ø")
                (lambda () (interactive) (find-file "~/Dropbox/org/okonomi.org")))

(use-package org-bullets
  :ensure t
  :config
  (add-hook 'org-mode-hook (lambda () (org-bullets-mode 1))))


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
                                      (electric-indent-mode t)
                                      (electric-pair-mode t))))
    (setq web-mode-enable-auto-quoting nil)))

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

;; RUBY 

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

;; HASKELL

(use-package haskell-mode
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'haskell-mode))

;; GO

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

;; RACKET

;; https://www.racket-mode.com/#racket_002dinsert_002dlambda
(use-package racket-mode
  :ensure t
  :config
  (setq racket-program "/usr/local/bin/racket")
  :config
  (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
  :config
  (add-hook 'racket-mode-hook (lambda ()
                                (electric-indent-mode t))))


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

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; Indent region better:
(global-set-key (kbd "M-i") 'indent-region)


;; https://www.emacswiki.org/emacs/AutoIndentation
(electric-indent-mode 1)



(defun toggle-comment-on-line ()
  "Comment or uncomment current line."
  (interactive)
  (if (region-active-p)
      (comment-or-uncomment-region (region-beginning) (region-end))
    (comment-or-uncomment-region (line-beginning-position) (line-end-position))))

(global-set-key (kbd "C-æ") 'toggle-comment-on-line)

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
 '(electric-indent-mode nil)
 '(minions-mode t)
 '(org-agenda-files (quote ("~/datainn/todo.org")))
 '(org-structure-template-alist
   (quote
    (("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse"))))
 '(package-selected-packages
   (quote 
    (gnuplot tide clj-refactor engine-mode company-tabnine company-flow flow-minor-mode projectile-ripgrep rg git-gutter+ git-gutter-+ add-node-modules-path web-mode flycheck-clj-kondo :nyan-mode company-auctex ox-latex ox-beamer auc-tex auctex eyebrowse org-tempo elfeed xref-js2 fireplace ace-window edit-indirect nyan-mode smart-hungry-delete hungry-delete expand-region minimap glsl-mode company-tern tern elm-yasnippets org-reveal minions dracula-theme solarized-theme neotree go-mode haskell-mode ruby-electric inf-ruby elm-mode try which-key use-package htmlize restclient yasnippet-snippets json-mode sml-mode markdown-mode tagedit rainbow-delimiters projectile paredit magit exec-path-from-shell clojure-mode-extra-font-locking cider)))
 '(save-place-mode t))
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


;;;

;; (require 'ac-geiser)
;; (add-hook 'geiser-mode-hook 'ac-geiser-setup)
;; (add-hook 'geiser-repl-mode-hook 'ac-geiser-setup)
;; (eval-after-load "auto-complete"
  ;; '(add-to-list 'ac-modes 'geiser-repl-mode))

;; (setq geiser-active-implementations '(chicken racket))

(show-paren-mode 1)

;; (add-to-list 'pretty-lambda-auto-modes 'geiser-repl-mode)
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


;;;;

(defun insert-current-date () (interactive)
    (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

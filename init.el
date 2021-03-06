;;;
;; EMACS config
;;;

;;; Code:

;; (byte-recompile-directory (expand-file-name "~/.emacs.d/elpa") 0)

(toggle-debug-on-error 1)
;; Less in the end of this file
(setq gc-cons-threshold (* 50 1000 1000))

(setq lexical-binding t)

(setq user-full-name "Fredrik Meyer"
      user-mail-address "hrmeyer@gmail.com")


(setq ns-pop-up-frames nil)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(setq locale-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; No need for ~ files when editing
(setq create-lockfiles nil
      auto-save-default nil
      create-lockfiles nil)
(setq default-directory (concat (getenv "HOME") "/"))
(setq custom-file (expand-file-name "custom.el" user-emacs-directory))
(when (file-exists-p custom-file)
  (load custom-file))

(unbind-key "C-z") ;; unbind the very annoying suspend-frame
(unbind-key "<mouse-2>")

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      select-enable-clipboard t)

(set-frame-font "Menlo-14")
;; Removes tool-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

(setq inhibit-startup-message t
      initial-scratch-message nil
      use-dialog-box nil)

(custom-set-variables '(calendar-week-start-day 1))

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)


(setq auth-source-debug 'trivia)
;; EDITING

;; Highlights matching parenthesis
(show-paren-mode 1)
(global-hl-line-mode 1)
(delete-selection-mode 1)

(setq c-default-style '((java-mode . "java")
                      (awk-mode . "awk")
                      (other . "stroustrup")))

;; First try to indent the current line, and if the line
;; was already indented, then try `completion-at-point'
(setq tab-always-indent 'complete)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)
(add-hook 'prog-mode-hook (lambda ()
                            (setq-local show-trailing-whitespace t)))

;; Dont show whitespaces in minibuffer
(add-hook 'minibuffer-setup-hook (lambda () (setq-local show-trailing-whitespace nil)))

(setq-default indicate-empty-lines 't)
(setq auth-sources '("/Users/fredrikmeyer/.authinfo"))

;; HippieExpand: M-n for å fullføre noe
;; http://www.emacswiki.org/emacs/HippieExpand
(setq hippie-expand-try-functions-list
      '(yas-hippie-try-expand
        try-expand-dabbrev
        try-expand-dabbrev-from-kill
        try-expand-dabbrev-all-buffers
        try-expand-whole-kill
        try-complete-file-name
        try-expand-line
        try-complete-lisp-symbol-partially
        try-complete-lisp-symbol))
(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "M-n") 'hippie-expand)

;; Utils

(setq tramp-use-ssh-controlmaster-options nil)

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;; end utils

;; Define package repositories
(require 'package)
;; (setq gnutls-algorithm-priority "NORMAL:-VERS-TLS1.3")
(package-initialize)

(setq package-enable-at-startup nil)

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

(custom-set-variables '(use-package-compute-statistics t))

;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (setq exec-path-from-shell-arguments nil)
  (exec-path-from-shell-initialize)
  (exec-path-from-shell-copy-envs
   '("PATH")))

(use-package server
  :defer 2
  :config
  (server-start))

(use-package system-packages
  :ensure t)

(use-package use-package-ensure-system-package
  :ensure t)

(use-package package-utils
  :ensure t)

;; https://github.com/quelpa/quelpa-use-package
(use-package quelpa-use-package
  :ensure t)

(use-package package-utils-upgrade-all-and-recompile
  :commands (package-utils-upgrade-all-and-recompile)

  :quelpa
  (package-utils-upgrade-all-and-recompile
   :fetcher gitlab
   :repo "ideasman42/emacs-package-utils-upgrade-all-and-recompile"))

(use-package benchmark-init
  :disabled
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

(use-package esup
  :ensure t
  :defer t
  ;; To use MELPA Stable use ":pin mepla-stable",
  :pin melpa-stable
  :commands (esup))

(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))

(use-package try
  :defer 2
  :ensure t)

(use-package which-key
  :ensure t
  :config
  (which-key-mode)
  (which-key-setup-side-window-bottom))

;; https://github.com/magnars/expand-region.le
(use-package expand-region
  :ensure t
  :bind ("C-=" . 'er/expand-region))

;; https://github.com/wakatime/wakatime-mode
;; See ~/.wakatime.cfg
(use-package wakatime-mode
  :defer 1
  :ensure t
  :hook (prog-mode global-wakatime)
  :config
  ;; Running on my Raspberry Pi
  (setq wakatime-api-key "c1af0bcd-9cfc-495a-ba27-2f66f5c308ef"))

(use-package flycheck
  :ensure t
  :defer 1
  :config
  (global-flycheck-mode)

  (setq flycheck-display-errors-delay 0.2)
  (setq eldoc-idle-delay 0.1)

  (add-hook 'flycheck-mode-hook 'add-node-modules-path)
  (flycheck-add-mode 'javascript-eslint 'web-mode)
  (flycheck-add-mode 'javascript-eslint 'vue-mode)
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)

  (setq flycheck-html-tidy-executable "/usr/local/Cellar/tidy-html5/5.6.0/bin/tidy")
  )

(use-package flycheck-color-mode-line
  :ensure t
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

(use-package flycheck-flow
  :defer 2
  :ensure t)

;; TODO: bytt ut med denne en gang? https://github.com/aaronjensen/eslintd-fix
(defun eslint-fix-file ()
  "Run eslint fix current file."
  (interactive)
  (cond ((locate-dominating-file default-directory "package-lock.json")
       (shell-command (concat "npm run eslint" " " "--fix" (buffer-file-name))))
        ((locate-dominating-file default-directory "yarn.lock")
         (call-process-shell-command
          (concat "yarn run eslint" " " "--fix " (buffer-file-name))
          nil "*Shell Command Output*" t))
        (t (message "No lock file.")))
  (revert-buffer t t)
  )

(defun eslint-fix-file-and-revert ()
  "Run eslint on current buffer."
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))

;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
(use-package paredit
  :defer 2
  :ensure t
  :config
  (dolist (m '(emacs-lisp-mode-hook
	       racket-mode-hook
	       racket-repl-mode-hook))
    (add-hook m #'paredit-mode))
  (bind-keys :map paredit-mode-map
	     ("{"   . paredit-open-curly)
	     ("}"   . paredit-close-curly))
  (unless terminal-frame
    (bind-keys :map paredit-mode-map
	       ("M-[" . paredit-wrap-square)
	       ("M-{" . paredit-wrap-curly))))

(use-package paredit-everywhere
  :defer 2
  :ensure t
  :hook (prog-mode . paredit-everywhere-mode))

(use-package flycheck-clj-kondo
  :defer 2
  :ensure t
  :after (flycheck))

;; key bindings and code colorization for Clojure
;; https://github.com/clojure-emacs/clojure-mode
(use-package clojure-mode
  :defer t
  :ensure t
  :mode "\\.edn$"
  :mode "\\.boot$"
  :mode "\\.clj$"
  :hook ((clojure-mode . subword-mode) ;; For Java class names
         (clojure-mode . electric-indent-mode)
         (clojure-mode . electric-pair-mode)
         (clojure-mode . paredit-mode))
  :config
  (require 'flycheck-clj-kondo)
  ;; Enable paredit for Clojure
  ;; (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojurescript-mode))
  (add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode)))

;; extra syntax highlighting for clojure
(use-package clojure-mode-extra-font-locking
  :defer 2
  :ensure t)

;; Integration with a Clojure REPL
;; https://github.com/clojure-emacs/cider
(use-package cider
  :defer 2
  :hook ((cider-mode . eldoc-mode)
         (cider-mode . company-mode)
         (cider-repl-mode . company-mode)
         (cider-mode . paredit-mode)
         (cider-repl-mode . paredit-mode))
  :config
  ;; go right to the REPL buffer when it's finished connecting
  (setq cider-repl-pop-to-buffer-on-connect t)

  ;; When there's a cider error, show its buffer and switch to it
  (setq cider-show-error-buffer t)
  (setq cider-auto-select-error-buffer t)

  ;; Wrap when navigating history.
  (setq cider-repl-wrap-history t)

  ;; Where to store the cider history.
  (setq cider-repl-history-file "~/.emacs.d/cider-history")

  ;; Don't prompt and don't save
  (setq cider-save-file-on-load nil)

  (defun cider-start-http-server ()
    (interactive)
    (cider-load-current-buffer)
    (let ((ns (cider-current-ns)))
      (cider-repl-set-ns ns)
      (cider-interactive-eval (format "(println '(def server (%s/start))) (println 'server)" ns))
      (cider-interactive-eval (format "(def server (%s/start)) (println server)" ns))))


  (defun cider-refresh ()
    (interactive)
    (cider-interactive-eval (format "(user/reset)")))

  (defun cider-user-ns ()
    (interactive)
    (cider-repl-set-ns "user"))
  (define-key clojure-mode-map (kbd "C-c C-v") 'cider-start-http-server)
  (define-key clojure-mode-map (kbd "C-M-r") 'cider-refresh)
  (define-key clojure-mode-map (kbd "C-c u") 'cider-user-ns)
  (define-key cider-mode-map (kbd "C-c u") 'cider-user-ns))

(use-package clj-refactor
  :ensure t
  :hook (clojure-mode . clj-refactor-mode)
  :config
  (add-hook 'clojure-mode-hook
            (lambda ()
              (cljr-add-keybindings-with-prefix "C-c C-m"))))

(use-package prolog-mode
  :defer 2
  :mode "\\.pl$")

(use-package restclient
  :defer 1
  :ensure t
  :mode "\\.http$\\'")

(use-package company-restclient
  :defer 1
  :ensure t
  :after restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

;; https://github.com/wasamasa/eyebrowse
(use-package eyebrowse
  :defer 1
  :ensure t
  :init
  (dotimes (n 10)
    (global-unset-key (kbd (format "C-%d" n)))
    (global-unset-key (kbd (format "M-%d" n))))
  (setq eyebrowse-keymap-prefix (kbd "C-C M-e"))
  :config
  (define-key eyebrowse-mode-map (kbd "M-1") 'eyebrowse-switch-to-window-config-1)
  (define-key eyebrowse-mode-map (kbd "M-2") 'eyebrowse-switch-to-window-config-2)
  (define-key eyebrowse-mode-map (kbd "M-3") 'eyebrowse-switch-to-window-config-3)
  (define-key eyebrowse-mode-map (kbd "M-4") 'eyebrowse-switch-to-window-config-4)
  (eyebrowse-mode t)
  (setq eyebrowse-post-window-switch-hook 'neo-global--attach)
  (setq eyebrowse-new-workspace t))

(use-package windmove
  :defer 2
  :ensure t
  :config
  (windmove-default-keybindings))

(use-package buffer-move
  :ensure t
  :bind
  (("C-c b u" . 'buf-move-up)
   ("C-c b d" . 'buf-move-down)
   ("C-c b l" . 'buf-move-left)
   ("C-c b r" . 'buf-move-right)))

(use-package fennel-mode
  :mode "\\.fnl$'"
  :ensure t)

(use-package counsel
  :after ivy
  :ensure t
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)
   ("M-x" . counsel-M-x)
   ("RET" . ivy-alt-done)
   ("C-x C-f" . counsel-find-file)
   ("C-h v" . counsel-describe-variable)
   ("C-h f" . counsel-describe-function)
   ("C-h o" . counsel-describe-symbol)
   ("<backspace>" . 'ivy-backward-delete-char))
  :config
  (counsel-mode)
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  (setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) "")
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-expression-history))

(use-package ivy
  :ensure t
  :defer 0.1
  :pin melpa
  :demand t
  :diminish (ivy-mode)
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer))
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
          (counsel-M-x . ivy--regex-plus)
          (t . ivy--regex-plus))))

;; https://github.com/tumashu/ivy-posframe
(use-package ivy-posframe
  :ensure t
  :config
  (ivy-posframe-mode t))

(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper-isearch)
	 ("C-r" . swiper-isearch)
	 ("C-x C-f" . counsel-find-file)))

;; https://github.com/Yevgnen/ivy-rich
(use-package ivy-rich
  :after ivy
  :hook (counsel-projectile-mode . ivy-rich-mode)
  :ensure t
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; https://github.com/asok/all-the-icons-ivy
(use-package all-the-icons-ivy-rich
  :hook (ivy-mode . all-the-icons-ivy-rich-mode)
  :ensure t)

;; http://www.emacswiki.org/emacs/SavePlace
(use-package saveplace
  :defer 1
  :ensure t
  :config
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places")))

;; https://github.com/hrs/engine-mode
(use-package engine-mode
  :disabled
  :ensure t
  :defer 2
  :config
  (engine-mode t)
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
  :defer 2
  :bind (("<backspace>" . smart-hungry-delete-backward-char)
	 ("C-d" . smart-hungry-delete-forward-char))
  :defer nil ;; dont defer so we can add our functions to hooks
  :config (smart-hungry-delete-add-default-hooks))

(use-package anzu
  :ensure t
  :defer 2
  :bind (("C-1" . anzu-query-replace)
         ("C-!" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode))

(use-package projectile
  :ensure t
  :defer t
  :config
  (define-key projectile-mode-map (kbd "M-p") 'projectile-command-map)
  (define-key projectile-mode-map (kbd "C-c p") 'projectile-command-map)
  (setq projectile-project-compilation-cmd ""))

; https://github.com/ericdanan/counsel-projectile
(use-package counsel-projectile
  :defer 1
  :ensure t
  :config
  (counsel-projectile-mode))

(use-package projectile-ripgrep
  :defer 1
  :ensure t)

;; https://github.com/dajva/rg.el
(use-package rg
  :defer 1
  :ensure t
  :ensure-system-package (rg . ripgrep)
  :config
  (rg-enable-default-bindings)
  (setq rg-executable "/usr/local/bin/rg"))

(use-package neotree
  :defer 1
  :ensure t
  :config
  (global-set-key (kbd "C-x t") 'neotree-toggle)
  (setq neo-smart-open t)
  (setq neo-theme 'icons)
  (setq neo-window-fixed-size nil)
  (setq neo-show-hidden-files t)
  ;; https://github.crookster.org/macOS-Emacs-26-display-line-numbers-and-me/
  (add-hook 'neo-after-create-hook (lambda (&rest _) (display-line-numbers-mode -1))))

;; https://github.com/domtronn/all-the-icons.el
(use-package all-the-icons
  :defer 1
  :ensure t)

;; https://github.com/jtbm37/all-the-icons-dired
(use-package all-the-icons-dired
  :ensure t
  :hook (dired-mode . all-the-icons-dired-mode))

(use-package rainbow-delimiters
  :defer 1
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package geiser
  :defer 2
  :ensure t)

(use-package ac-geiser
  :defer 2
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
  :defer 1
  :ensure t)

;;; dont know what this is??
(use-package strokes
  :disabled
  :config
  (strokes-mode 1))

(use-package undo-tree
  :ensure t
  :disabled
  :init
  (global-undo-tree-mode)
  :config
  (undo-tree))

(use-package magit
  :ensure t
  :defer 1
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-list-repositories)
         ("C-x C-S-B" . magit-blame-addition))
  :config
  (setq magit-repository-directories
        `(("~/code" . 1)
          ("~/code/work" . 1)
          (,user-emacs-directory . 0)))
  (setq magit-list-refs-sortby "-creatordate"))

(use-package forge
  :defer 1
  :ensure t
  :after magit
  :config
  (setq forge-topic-list-order '(number . >)))

;; https://github.com/alphapapa/magit-todos#installation
(use-package magit-todos
  :ensure t
  :config
  (magit-todos-mode t))

;; https://github.com/syohex/emacs-git-messenger
(use-package git-messenger
  :defer 1
  :ensure t
  :bind ("C-c M" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

;; https://github.com/nonsequitur/git-gutter-plus
(use-package git-gutter+
  :defer 2
  :ensure t
  :config
  (global-git-gutter+-mode))

(use-package ob-ipython
  :disabled
  :after company
  :ensure t
  :config
  (add-hook 'ob-ipython-mode-hook
            (lambda () (company-mode 1))))

;; https://github.com/zweifisch/ob-http
;; org mode source block http
(use-package ob-http
  :ensure t)

;; https://github.com/alphapapa/org-super-agenda/#installation
(use-package org-super-agenda
  :ensure t
  :config
  (org-super-agenda-mode t)
  (setq org-super-agenda-groups
        '(
          (:name "Important"
                 :priority "A")
          (:category "notater" :name "Notater")
          (:category "audio_xal" :name "Work")
          )))

(use-package org
  :defer 1
  :ensure org-plus-contrib
  :pin org
  :config
  (org-clock-persistence-insinuate)
  (setq org-clock-persist t)
  (add-hook 'org-mode-hook (lambda ()
                             (visual-line-mode t)
                             (auto-save-mode t)
                             (electric-pair-mode 0)))

  (setq org-src-fontify-natively t
        org-src-tab-acts-natively nil
        org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0)
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))
  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'org-preview-latex-fragment nil 'make-it-local)))

  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
                               ;; (ipython . t)
                               (calc . t)
                               (clojure . t)
                               (shell . t)
                               (plantuml . t)
                               (http . t)
                               (gnuplot . t)))
  (setq org-plantuml-exec-mode 'plantuml)
  (setq org-plantuml-jar-path "/usr/local/bin/plantuml")
  (setq org-babel-clojure-backend 'cider)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  (setq org-refile-targets '((nil :maxlevel . 3)))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)

  (global-set-key (kbd "C-c a") 'org-agenda)
  (setq org-agenda-files (list "~/Dropbox/org/audio_xal.org"
                               "~/Dropbox/org/tasks.org"
                               "~/Dropbox/org/notater.org"))

  ;; https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
  (setq org-agenda-custom-commands
        '(("w" "Work view"
           (
            (todo "" ((org-super-agenda-groups
                       '(
                         ;; (:name "First prio"
                         ;;        :category "audio_xal" :priority "A")
                         (:name "Top prio"
                                :priority "A")
                         (:auto-outline-path t)
                         )))))
           ((org-agenda-files '("~/Dropbox/org/audio_xal.org")))
           )))


  (setq org-default-notes-file "~/Dropbox/org/daglige_notater.org")
  (global-set-key (kbd "C-c c") 'org-capture)
  (setq org-capture-templates
        '(
          ("n" "Note" entry (file "~/Dropbox/org/daglige_notater.org") "* %U\n%?")
          ("d" "Dagbok" entry (file "~/Dropbox/org/dagbok.org")  "** %t\n%?")
          ("a" "Audio project" entry
           (file+headline "~/Dropbox/org/audio_xal.org" "Usorterte todos")
           "* TODO %?\n")
          ("t" "Todo" entry (file "~/Dropbox/org/tasks.org")
           "* TODO %?\n%U" :empty-lines 1)))
  (require 'org-tempo)
  (require 'ox-md))


(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;; In order for org mode / gnuplot to work
(use-package gnuplot
  :defer 2
  :ensure t)

(use-package org-roam
      :ensure t
      :hook
      (after-init . org-roam-mode)
      :custom
      (org-roam-directory "~/Dropbox/org/roam/")
      :bind (:map org-roam-mode-map
                  (("C-c n l" . org-roam)
                   ("C-c n r" . org-roam-buffer-toggle-display)
                   ("C-c n f" . org-roam-find-file)
                   ("C-c n g" . org-roam-graph))
                  :map org-mode-map
                  (("C-c n i" . org-roam-insert))
                  (("C-c n I" . org-roam-insert-immediate)))
      :config
      (setq org-roam-completion-everywhere t)
      (require 'org-roam-protocol))

;; https://github.com/IvanMalison/org-projectile
(use-package org-projectile
  :disabled
  :defer 3
  :ensure t
  :config
  (setq org-projectile-projects-file "~/Dropbox/org/prosjekter.org")
   (add-to-list 'org-capture-templates
                 (org-projectile-project-todo-entry
                  :capture-character "l")))

(use-package treemacs
  :ensure t
  :bind
  (:map global-map
         ;; ("M-0"       . treemacs-select-window)
        ("C-z 1"   . treemacs-delete-other-windows)
        ("C-z z z"   . treemacs)
        ("C-z z B"   . treemacs-bookmark)
        ("C-z z C-t" . treemacs-find-file)
        ("C-z t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :after treemacs magit
  :ensure t)

(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file "~/Dropbox/org/notater.org")))

(global-set-key (kbd "C-c n")
                (lambda () (interactive) (find-file "~/Dropbox/org/daglige_notater.org")))

(global-set-key (kbd "C-c ø")
                (lambda () (interactive) (find-file "~/Dropbox/org/okonomi.org")))

(global-set-key (kbd "C-c i")
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))

(global-set-key (kbd "C-c d")
                (lambda () (interactive) (find-file "~/Dropbox/org/dagbok.org")))


(use-package company
  :defer
  :ensure t
  :hook (after-init . global-company-mode)
  :custom (company-dabbrev-downcase nil)
  :config
  ;; Don't set this to 0 if you want yasnippet to work well.
  (setq company-idle-delay 0.1)
  (setq company-minimum-prefix-length 2)
  (add-to-list 'company-backends 'company-flow))

(use-package company-quickhelp
  :after company
  :ensure t
  :config
  (company-quickhelp-mode))

;; https://github.com/sebastiencs/company-box
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package company-flow
  :defer 2
  :ensure t)

(use-package json-mode
  :defer 2
  :ensure t)

(use-package prettier-js
  :defer 2
  :ensure t)

(use-package web-mode
  :ensure t
  :after (add-node-modules-path)
  :mode "\\.[t|j]sx?$" ;; autoenable for js, jsx, ts, tsx
  :mode "\\.tsx?$\\'"
  :bind ("C-c r" . 'tide-rename-symbol-at-location)
  :config
  (setq web-mode-indentation-params '(("lineup-calls" . 1)))
  ;; (setq web-mode-indentation-params '())
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?$\\'")))
  (setq web-mode-enable-auto-indentation nil)
  (add-hook 'web-mode-hook (lambda ()
                             (tide-setup)
                             (tide-hl-identifier-mode t)
                             ;; (local-set-key (kbd "C-c r") 'tide-rename-symbol-at-location)
                             (flycheck-mode 1)
                             (yas-activate-extra-mode 'js2-mode)
                             (if (string= (file-name-extension buffer-file-name) "ts")
                                 (flycheck-add-mode 'typescript-tide 'web-mode))
                             (when (and (or (locate-dominating-file default-directory ".prettier.rc")
                                            (locate-dominating-file default-directory ".prettierrc.json")
                                            (locate-dominating-file default-directory ".prettierrc")))
                                                              ;; (string= (file-name-extension buffer-file-name) "ts")
                               (prettier-js-mode 1)
                               (add-hook 'before-save-hook 'prettier-js nil t)
                               ;; (add-hook 'before-save-hook 'tide-format-before-save)
                               )
                             (when (locate-dominating-file default-directory ".eslintrc.js")
                               (flycheck-add-mode 'javascript-eslint 'web-mode))
                             (electric-indent-mode nil)
                               ;; (add-hook 'after-save-hook #'eslint-fix-file-and-revert)

                             (electric-pair-mode t)))
  (setq web-mode-enable-auto-quoting nil))

(use-package typescript
  :defer 1
  :ensure t)

(use-package tide
  :defer 1
  :bind ("C-c r" . tide-rename-symbol)
  :ensure t)

(use-package plantuml-mode
  :defer 1
  :ensure t
  :config
  (setq plantuml-executable-path "/usr/local/bin/plantuml")
  (setq plantuml-output-type "png")
  (setq plantuml-default-exec-mode 'executable))

(use-package rust-mode
  :defer 2
  :ensure t
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'rust-mode-hook (lambda () (add-hook 'before-save-hook 'lsp-format-buffer nil t))))

(use-package flycheck-rust
  :disabled
  :ensure t
  :config
  (setq flycheck-rust-cargo-executable "/Users/fredrikmeyer/.cargo/bin/cargo")
  (add-hook 'flycheck-mode-hook #'flycheck-rust-setup))

;; https://github.com/kwrooijen/cargo.el
(use-package cargo
  :ensure t
  :hook ((rust-mode . cargo-minor-mode)))

(defun setup-vue-with-ts ()
  "Setup vue."
  (interactive)
  (tide-setup)
  (eldoc-mode +1)
  (when (flycheck-may-enable-checker 'javascript-eslint)
    (flycheck-select-checker 'javascript-eslint))
  (tide-hl-identifier-mode t))

(use-package vue-mode
  :ensure t
  :mode "\\.vue\\'"
  :config
  (setq mmm-typescript-mode-submode-hook #'setup-vue-with-ts)
  (set-face-background 'mmm-default-submode-face nil)
  (setq css-indent-offset 2))

(use-package smartparens
  :ensure t
  :defer 1
  :hook ((web-mode . smartparens-mode)
         (python-mode . smartparens-mode))
  :config
  (require 'smartparens-config))

;; https://elpa.gnu.org/packages/sml-mode.html
(use-package sml-mode
  :defer 2
  :ensure t)

(use-package flow-minor-mode
  :defer 2
  :disabled
  :ensure t
  :hook (web-mode . flow-minor-enable-automatically))

(use-package add-node-modules-path
  :defer 1
  :ensure t)

(use-package js2-mode
  :disabled
  :ensure t
  :config
  (setq-default indent-tabs-mode nil)
  (setq js2-strict-missing-semi-warning nil)
  (add-hook 'js-mode-hook 'js2-minor-mode)
  (add-to-list 'auto-mode-alist '("\\.js\\'" . js2-mode))
  (add-hook 'js-mode-hook 'electric-pair-mode)
  (add-hook 'js-mode-hook 'electric-indent-mode))

(use-package tern
  :disabled
  :ensure t)

(use-package company-tabnine
  :defer 2
  :ensure t
  :config
  (add-hook 'python-mode-hook
            '(lambda ()
               (add-to-list (make-local-variable 'company-backends) #'company-tabnine))))


(use-package company-tern
  :disabled
  :defer 1
  :ensure t
  :after tern
  :init
  (add-hook 'js2-mode-hook
            (lambda ()
              (add-to-list (make-local-variable 'company-backends)
                           '(company-tern company-dabbrev))
              (setq company-idle-delay 0)
              (tern-mode t)
              (company-mode))))

(use-package glsl-mode
  :defer 2
  :ensure t
  :mode "\\.glsl\\'")

(use-package elm-mode
  :defer 2
  :ensure t
  :disabled ;; dont use elm anymore
  :config
  (setq-default indent-tabs-mode nil)
  (add-hook 'elm-mode-hook 'electric-pair-mode)
  (add-hook 'elm-mode-hook 'electric-indent-mode)
  (add-hook 'elm-mode-hook #'elm-oracle-setup-completion)
  (add-hook 'elm-mode-hook 'company-mode)
  (setq elm-format-on-save t)
  (setq elm-interactive-command '("elm" "repl")
        elm-reactor-command '("elm" "reactor")
        elm-reactor-arguments '("--port" "8000")
        elm-compile-command '("elm" "make")
        elm-compile-arguments '("--output=elm.js" "--debug")
        elm-package-command '("elm" "package")
        elm-package-json "elm.json"))

(use-package elm-yasnippets
  :disabled
  :defer 3
  :ensure t)

;; Ruby
(use-package ruby-mode
  :defer 2
  :ensure t)

(use-package ruby-electric
  :defer 2
  :ensure t
  :hook ruby-mode)

;; https://github.com/JoshCheek/seeing_is_believing
;; https://github.com/jcinnamond/seeing-is-believing
(use-package seeing-is-believing
  :defer 2
  :ensure t
  :hook ruby-mode)

(use-package inf-ruby
  :defer 2
  :ensure t
  :hook (ruby-mode . inf-ruby-minor-mode))

;; Haskell
(use-package haskell-mode
  :defer 2
  :ensure t
  :config
  (add-hook 'haskell-mode-hook 'haskell-mode)
  (setq haskell-process-type 'stack-ghci)
  (add-hook 'haskell-mode-hook
            (lambda ()
              (set (make-local-variable 'company-backends)
                   (append '((company-capf company-dabbrev-code))
                           company-backends)))))

;; Go
(use-package go-mode
  :ensure t
  :defer 2
  :config
  (add-hook 'before-save-hook 'gofmt-before-save)
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  (add-hook 'go-mode-hook 'electric-pair-mode)
  (add-hook 'go-mode-hook 'electric-indent-mode)
  (add-hook 'go-mode-hook (lambda ()
            (setq tab-width 4))))

;; https://www.racket-mode.com
(use-package racket-mode
  :defer 2
  :ensure t
  :config
  (setq racket-program "/usr/local/bin/racket")
  (add-to-list 'auto-mode-alist '("\\.rkt\\'" . racket-mode))
  (add-hook 'racket-mode-hook
            (lambda ()
              (electric-indent-mode t)
              (electric-pair-mode t)
              )))

(use-package dockerfile-mode
  :defer 2
  :ensure t
  :mode "Dockerfile\\'")

(use-package minimap
  :defer 2
  :ensure t
  :bind ("C-x w" . minimap-mode)
  :config
  (setq minimap-window-location 'right)
  (setq minimap-automatically-delete-window nil))

(use-package helpful
  :defer 2
  :ensure t
  :after counsel
  :config
  (setq counsel-describe-function-function #'helpful-callable
	counsel-describe-variable-function #'helpful-variable)

  (global-set-key (kbd "C-h k") #'helpful-key)
  (global-set-key (kbd "C-c C-d") #'helpful-at-point))


(use-package markdown-mode
  :defer 2
  :ensure t
  :mode ("\\.md[x]?$")
  :config
  (add-hook 'markdown-mode-hook 'visual-line-mode)
  (add-hook 'markdown-mode-hook
            (lambda ()
              (let ((file (file-name-nondirectory buffer-file-name)))
                (format "pandoc -o %s.pdf %s --pdf-engine=xelatex"
                        (file-name-sans-extension file)
                        file))))
  (setq markdown-header-scaling 1)
  (setq markdown-command
      (concat
       "/usr/local/bin/pandoc"
       " --from=markdown --to=html"
       " --standalone --mathjax --highlight-style=pygments")))


;; https://github.com/TobiasZawada/texfrag
(use-package texfrag
  :ensure t
  :config
  (add-hook 'markdown-mode-hook 'texfrag-mode))

(use-package pdf-tools
  ;; :load-path "pdf-tools"
  :disabled ;; takes a lot of startup time
  :pin manual
  :config
  (setq pdf-info-epdfinfo-program "/usr/local/bin/epdfinfo")
  (setq pdf-view-display-size 'fit-page)
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
  (setq pdf-view-use-unicode-ligther nil)
  ;; (pdf-tools-install)
  (pdf-loader-install)
  )

;; Automatically refreshes PDF
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
(add-hook 'doc-view-mode-hook (lambda () (linum-mode 0)))
;; Requires `brew install ghostscript`


;; (byte-recompile-directory "~/.emacs.d/elpa" 0 t)

(use-package lsp-mode
  :defer 2
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :config
  (local-set-key (kbd "M-.") 'lsp-find-definition)
  (setq lsp-modeline-code-actions-segments '(count icon name))

  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 5 1024 1024)) ;; 5mb
  (setq gc-cons-threshold 100000000)

  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  ;; (add-hook 'python-mode-hook #'lsp)

  (require 'lsp-rust)
  (require 'lsp-csharp)
  (require 'lsp-pyright)
  (setq lsp-rust-server 'rust-analyzer)

  ;; To enable mypy
  ;; https://github.com/tomv564/pyls-mypy
  ;; Also had to install this from source:
  ; pip3 install git+https://github.com/tomv564/pyls-mypy.git
  (lsp-register-custom-settings '(("pyls.plugins.pyls_mypy.enabled" t t)))
  (lsp-register-custom-settings '(("pyls.plugins.pyls_isort.enabled" t t))))

(use-package lsp-ui
  :defer 2
  :ensure t
  :config
  (setq lsp-ui-sideline-delay 0.1))
  ;; (setq lsp-ui-doc-position at))

(use-package lsp-java
  :disabled
  :ensure t
  :after lsp
  :config
  (add-hook 'java-mode-hook 'lsp))

(use-package dap-mode
  :disabled
  :ensure t
  :after lsp-mode
  :hook ((lsp-mode . dap-mode)
         (lsp-mode . dap-ui-mode))
  :config
  ;; Maybe solves it...
  (dap-tooltip-mode -1)
  ;; (require 'dap-python)
  ; should fix it also
;  (dap-enable-mouse-support nil)
  ;; Will break tooltips: https://github.com/emacs-lsp/dap-mode/issues/314
  )


;; LATEX

(use-package company-auctex
  :defer 2
  :ensure t
  :config
  (add-hook 'latex-mode-hook (company-auctex-init))
  (setq TeX-auto-save t)
  (setq TeX-parse-self t))

;; Tabs and ribbons for the mode line
;; https://github.com/tarsius/moody
(use-package moody
  :defer 2
  :ensure t
  :disabled
  :config
  (setq x-underline-at-descent-line t)
  (moody-replace-mode-line-buffer-identification)
  (moody-replace-vc-mode))

(use-package doom-modeline
  :ensure t
  :hook (after-init . doom-modeline-mode)
  :config
  (setq doom-modeline-buffer-encoding nil)
  (setq doom-modeline-minor-modes t)
  (setq doom-modeline-buffer-file-name-style 'buffer-name))

;; https://github.com/johanvts/emacs-fireplace
(use-package fireplace
  :defer 2
  :ensure t)

;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

(use-package spacemacs-common
  :ensure spacemacs-theme
  :disabled
  :config
  (load-theme 'spacemacs-dark t))

(use-package leuven-theme
  :ensure t
  :disabled
  :config
  (load-theme 'leuven t))

(use-package modus-operandi-theme
  :ensure t
  :config
  (load-theme 'modus-operandi t)
  (setq modus-operandi-theme-rainbow-headings t)
  (setq modus-operandi-theme-scale-headings t))

(use-package modus-vivendi-theme
  :ensure t)

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

;; https://emacsredux.com/blog/2013/05/31/highlight-lines-that-exceed-a-certain-length-limit/
(use-package whitespace
  :defer 2
  :ensure t
  :hook (python-mode . whitespace-mode)
  :config
  (setq whitespace-line-column 80)
  (setq whitespace-style '(face lines-tail)))

;; This package implements a menu that lists enabled minor-modes, as well as
;; commonly but not currently enabled minor-modes.
;; https://github.com/tarsius/minions
(use-package minions
  :defer 1
  :ensure t
  :config (minions-mode 1))

;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :ensure t
  :defer 3
  :hook (prog-mode . yas-global-mode))

(use-package yasnippet-snippets
  :defer 3
  :ensure t
  :after yasnippet)

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :defer 1
  :ensure t
  :config
  (global-set-key (kbd "C-S-c C-S-c") 'mc/edit-lines)
  (global-set-key (kbd "C->") 'mc/mark-next-like-this)
  (global-set-key (kbd "C-<") 'mc/mark-previous-like-this)
  (global-set-key (kbd "C-c C-<") 'mc/mark-all-like-this))

(use-package elfeed
  :defer 1
  :ensure t
  :init
  (setq elfeed-feeds
        '(
          ("http://blag.xkcd.com/feed/" blog)
          ("https://xkcd.com/rss.xml" blog cartoon)
          ("http://jvns.ca/atom.xml" blog)
          ("https://www.johndcook.com/blog/feed" blog math)
          ("https://nhigham.com/feed/" blog math)
          ("https://www.newth.net/eirik/feed/" blog)
          ("https://nullprogram.com/feed/" blog)
          ("https://nrkbeta.no/feed/" blog tech)
          ("https://slatestarcodex.com/feed/" blog skeptic)
          ("https://lichess.org/blog.atom" chess blog)
          ("http://bit-player.org/feed" math blog)
          ("http://www.jeffgeerling.com/blog.xml" raspberry blog)
          ("https://jvns.ca/atom.xml" blog)
          ("https://aws.amazon.com/blogs/opensource/feed/" aws blog)
          ("http://www.realtimerendering.com/blog/feed/" blog graphics)
          ("https://0fps.net/feed/" blog graphics)
          ("https://golem.ph.utexas.edu/category/atom10.xml" blog math)
          ("https://qchu.wordpress.com/feed/" math)
          ("https://karthinks.com/software/index.xml" emacs)
          ("https://www.math.columbia.edu/~woit/wordpress" math physics)
          ))
  :config
  (defun elfeed-open-maybe-in-xwidget (&optional use-generic-p)
    (interactive "P")
    (let ((browse-url-browser-function #'xwwp))
      (elfeed-show-visit use-generic-p)))
  (define-key elfeed-show-mode-map (kbd "B") 'elfeed-open-maybe-in-xwidget)
  )

(use-package xwwp
  :load-path "~/.emacs.d/xwwp"
  :ensure t)

(use-package xwwp-follow-link
  :load-path "~/.emacs.d/xwwp-follow-link"
  :custom
  (xwwp-follow-link-completion-system 'ivy)
  :bind (:map xwidget-webkit-mode-map
              ("v" . xwwp-follow-link)))

(global-set-key (kbd "C-å") (lambda ()
                              (interactive)
                              (xwwp (thing-at-point 'url 'no-properties))))

;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :ensure t
  :config
  (dashboard-setup-startup-hook)
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 5)
                          (projects . 10)
                          (bookmarks . 5)
                          (agenda . 5))))

;; https://github.com/gonewest818/dimmer.el
(use-package dimmer
  :disabled
  :defer 1
  :ensure t
  :config
  (dimmer-configure-which-key)
  (setq dimmer-prevent-dimming-predicates '(window-minibuffer-p))
  (setq dimmer-buffer-exclusion-regexps '("magit-diff"
                                          "*LV*"
                                          "\\*\\(LV\\|transient\\)\\*"
                                          "^*Messages*"
                                          "^\\*Minibuf-[0-9]+\\*"
                                          "transient"
                                          ".*Minib.*"
                                          "^.\\*Echo.*\\*"
                                          "Help"))
  (setq dimmer-fraction 0.3) ;; Originally 0.2
  (dimmer-mode t))

;; https://www.emacswiki.org/emacs/WinnerMode
;; Winner Mode is a global minor mode. When activated, it allows you to “undo” (and “redo”) changes in the window configuration with the key commands ‘C-c left’ and ‘C-c right’
(winner-mode t)

(global-set-key (kbd "C-S-c <left>") 'shrink-window-horizontally)
(global-set-key (kbd "C-S-c <right>") 'enlarge-window-horizontally)
(global-set-key (kbd "C-S-c <up>") 'enlarge-window)
(global-set-key (kbd "C-S-c <down>") 'shrink-window)


;; Yaml
(use-package yaml-mode
  :defer 1
  :ensure t
  :config
  (add-hook 'yaml-mode-hook
            (lambda () (define-key yaml-mode-map (kbd "<C-return>") 'newline-and-indent)))
  (setq yaml-indent-offset 2))

(use-package julia-mode
  :defer 2
  :ensure t)

(use-package csharp-mode
  :disabled
  :defer 2
  :ensure t
  :mode "\\.cs$"
  :config
  (add-hook 'csharp-mode-hook #'lsp)
  (defun my-csharp-mode-setup ()
    (omnisharp-mode)
    (flycheck-mode 1))

  ;; (add-hook 'csharp-mode-hook 'my-csharp-mode-setup t)
  )

(use-package omnisharp
  :disabled
  :defer 2
  :ensure t
  :config
  (setq omnisharp-debug nil))

(use-package dired-subtree
  :defer 2
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)
              ("<tab>" . dired-subtree-toggle))
  :config
  (setq dired-subtree-use-backgrounds nil))

;; Se på https://github.com/purcell/ibuffer-projectile?
(use-package ibuffer
  :bind ("C-x C-b" . ibuffer))


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

;; "When several buffers visit identically-named files,
;; Emacs must give the buffers distinct names. The usual method
;; for making buffer names unique adds ‘<2>’, ‘<3>’, etc. to the end
;; of the buffer names (all but one of them).
;; The forward naming method includes part of the file's directory
;; name at the beginning of the buffer name
;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Uniquify.html

(use-package uniquify
  :defer 1
  :config
  (setq uniquify-buffer-name-style 'forward))

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(use-package recentf
  :defer t
  :ensure t
  :config
  (setq recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-mode 1)
  (setq recentf-max-saved-items 50)
  (setq recentf-max-menu-items 200))

(setq ediff-window-setup-function 'ediff-setup-windows-plain)


(setq-default cursor-type 'bar)

;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

;; (setq system-uses-terminfo nil)


(setq sgml-quick-keys 'close)


;; Automatically load paredit when editing a lisp file
;; More at http://www.emacswiki.org/emacs/ParEdit
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)
(add-hook 'ielm-mode-hook 'turn-on-eldoc-mode)


;; Show line numbers
(global-display-line-numbers-mode 1)
(column-number-mode 1)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; increase font size for better readability
(set-face-attribute 'default nil :height 140)
(setq initial-frame-alist '((top . 0) (left . 0) (width . 177) (height . 53)))

;; These settings relate to how emacs interacts with your operating system
(setq ;; makes killing/yanking interact with the clipboard
      ;; I'm actually not sure what this does but it's recommended?
      select-enable-primary t

      ;; Save clipboard strings into kill ring before replacing them.
      ;; When one selects something in another program to paste it into Emacs,
      ;; but kills something in Emacs before actually pasting it,
      ;; this selection is gone unless this variable is non-nil
      save-interprogram-paste-before-kill t

      ;; Shows all options when running apropos. For more info,
      ;; https://www.gnu.org/software/emacs/manual/html_node/emacs/Apropos.html
      apropos-do-all t

      ;; Mouse yank commands yank at point instead of at click.
      mouse-yank-at-point t)

;; No cursor blinking, it's distracting
(blink-cursor-mode 0)

;; Full path in title bar
(setq-default frame-title-format "Emacs %b (%f)")

;; don't pop up font menu
(global-set-key (kbd "s-t") '(lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)
(setq visible-bell t)


(show-paren-mode 1)

(pretty-lambda-for-modes)

(dolist (mode pretty-lambda-auto-modes)
  ;; add paredit-mode to all mode-hooks
  (add-hook (intern (concat (symbol-name mode) "-hook")) 'paredit-mode))


(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))



;;;;;;;;;; Oz mode

;; (or (getenv "OZHOME")
;;     (setenv "OZHOME"
;;             "/Applications/Mozart2.app/Contents/Resources/"))   ; or wherever Mozart is installed
;; (setenv "PATH" (concat (getenv "OZHOME") "/bin:" (getenv "PATH")))

;; (setq load-path (cons (concat (getenv "OZHOME") "/share/mozart/elisp")
;;                       load-path))

;; (setq auto-mode-alist
;;       (append '(("\\.oz\\'" . oz-mode)
;;                 ("\\.ozg\\'" . oz-gump-mode))
;;               auto-mode-alist))

;; (autoload 'run-oz "oz" "" t)
;; (autoload 'oz-mode "oz" "" t)
;; (autoload 'oz-gump-mode "oz" "" t)
;; (autoload 'oz-new-buffer "oz" "" t)
;; (add-hook 'oz-mode-hook 'electric-pair-mode 'electric-indent-mode)


(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
(use-package fm-common-lisp)
(use-package fm-python)

;;;; Useful functions

(defun insert-current-date ()
  "Insert current date."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +%Y-%m-%d)")))

(defun insert-current-date-with-weekday ()
  "Insert current date with weekday."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +'%Y-%m-%d %A')")))


(defun die-tabs ()
  "Replace all tabs in buffer with spaces."
  (interactive)
  (mark-whole-buffer)
  (set-variable 'tab-width 4)
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

(defun format-and-save-project ()
  "Format current buffer and save all open buffers."
         (interactive)
         (let* ((project (projectile-ensure-project (projectile-project-root)))
                (project-name (projectile-project-name project))
                (modified-buffers (cl-remove-if-not (lambda (buf)
                                                      (and (buffer-file-name buf) (buffer-modified-p buf)))
                                                    (projectile-project-buffers project))))
           (if (null modified-buffers)
               (message "[%s] No buffers need saving" project-name)
             (dolist (buf modified-buffers)
               (with-current-buffer buf
                 (message (buffer-name buf))
                 (when (lsp-feature? "textDocument/formatting")
                   (message "got here with " (buffer-name buf))
                   (lsp-format-buffer))
                 (save-buffer)))
             (message "[%s] Saved and formatted %d buffers" project-name (length modified-buffers)))))

;; https://emacsredux.com/blog/2013/04/02/move-current-line-up-or-down/
(defun move-line-up ()
  "Move up the current line."
  (interactive)
  (transpose-lines 1)
  (forward-line -2)
  (indent-according-to-mode))

(defun move-line-down ()
  "Move down the current line."
  (interactive)
  (forward-line 1)
  (transpose-lines 1)
  (forward-line -1)
  (indent-according-to-mode))

(global-set-key [(meta up)]  'move-line-up)
(global-set-key [(meta down)]  'move-line-down)

(setq gc-cons-threshold (* 2 1000 1000))

;;; init.el ends here

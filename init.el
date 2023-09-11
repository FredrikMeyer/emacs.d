;;; init.el --- My Emacs config -*- lexical-binding: t; -*-x

;; Author: Fredrik Meyer

;;; Commentary:

;; My personal messy Emacs config.

;;; Code:

;; (byte-recompile-directory (expand-file-name "~/.emacs.d/") 0)
;; (profiler-start 'cpu+mem)

;; Define package repositories

(package-initialize)

(add-to-list 'package-archives
             '("gnu" . "https://elpa.gnu.org/packages/"))

(add-to-list 'package-archives
             '("melpa" . "https://melpa.org/packages/"))

(add-to-list 'package-archives
             '("nongnu" . "https://elpa.nongnu.org/nongnu/"))


(toggle-debug-on-error)
(require 'package)
(assq-delete-all 'org package--builtins)
(assq-delete-all 'org package--builtin-versions)

(unless (package-installed-p 'use-package)
  (message "Refreshing package contents...")
  (package-refresh-contents)
  (package-install 'use-package))

(setq use-package-verbose nil)

;; (toggle-debug-on-error 1)

(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook (lambda () (setq gc-cons-threshold 800000)))

(setq lexical-binding t)

(setq user-full-name "Fredrik Meyer"
      user-mail-address "hrmeyer@gmail.com")

;; (setq comp-async-report-warnings-errors nil)

;; Full path in title bar
(setq-default frame-title-format "Emacs %b (%f)")

;; (setq ns-pop-up-frames nil)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)
(prefer-coding-system 'utf-8)

;; No need for ~ files when editing
(setq create-lockfiles nil
      auto-save-default nil
      default-directory (concat (getenv "HOME") "/")
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      )

(when (file-exists-p custom-file)
  (load custom-file))

;; (unbind-key "C-z") ;; unbind the very annoying suspend-frame
;; (unbind-key "<mouse-2>")

(tool-bar-mode -1)

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      mac-function-modifier 'super
      select-enable-clipboard t
      inhibit-startup-message t
      initial-scratch-message nil
      use-dialog-box nil
      split-width-threshold 160
      split-height-threshold 80)

(custom-set-variables '(calendar-week-start-day 1))

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)

;; (setq auth-source-debug 'trivia)

;; EDITING

;; Highlights matching parenthesis
(show-paren-mode 1)
(global-hl-line-mode 1)
(delete-selection-mode 1)

;; Show line numbers
(global-display-line-numbers-mode 1)
(column-number-mode 1)
(setq-default cursor-type 'bar)

(setq c-default-style '((java-mode . "java")
                        (awk-mode . "awk")
                        (other . "stroustrup")))

;; First try to indent the current line, and if the line
;; was already indented, then try `completion-at-point'
(setq tab-always-indent t)
(setq tab-first-completion nil)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)
(add-hook 'prog-mode-hook (lambda ()
                            (setq-local show-trailing-whitespace t)))

;; Don't show whitespaces in minibuffer
(add-hook 'minibuffer-setup-hook
          (lambda () (setq-local show-trailing-whitespace nil)))

(setq-default indicate-empty-lines 't)

(setq auth-sources '("~/.authinfo"))

;; HippieExpand: M-n for to complete
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

(global-set-key (kbd "M-n") 'hippie-expand)

;; From the docs: Set it to nil, if you use Control* or Proxy* options in
;;your ssh configuration. (I do)
(setq tramp-use-ssh-controlmaster-options nil)


(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))

;; (add-to-list 'package-archives
;;              '("melpa-stable" . "http://stable.melpa.org/packages/"))

;; (add-to-list 'package-archives
;;              '("org" . "https://orgmode.org/elpa/"))


(add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(eval-when-compile
  (require 'use-package))

;; (use-package crux
;;   :ensure t)


(setenv "SHELL" "/usr/bin/zsh")
;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  ;; :if (memq window-system '(mac ns x))
  :ensure t
  :config
  (setenv "SHELL" "/usr/bin/zsh")
  (setq exec-path-from-shell-arguments '("-l" "-i"))
  (setq exec-path-from-shell-shell-name "/usr/bin/zsh")
  (setq exec-path-from-shell-variables '("PATH" "MANPATH" "WORKON_HOME" "PYTHONPATH" "TS_DATA_SERVICE_PORT"))
  (setenv "WORKON_HOME" "~/miniconda/envs")
  (exec-path-from-shell-initialize))

;; (use-package server
;;   :defer 2
;;   :config
;;   (server-start))

;; https://gitlab.com/jabranham/system-packages
(use-package system-packages
  :ensure t)

(use-package use-package-ensure-system-package
  :ensure t)

(use-package package-utils
  :ensure t)

;; https://github.com/quelpa/quelpa-use-package
;; (use-package quelpa-use-package
;;   :ensure t)

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
  ;;  :pin melpa-stable
  :commands (esup))

;; https://github.com/emacsorphanage/popwin
(use-package popwin
  :ensure t
  :config
  (popwin-mode 1))

(use-package try
  :commands try
  :defer 5
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

;; https://github.com/leoliu/easy-kill
(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))

(use-package flycheck
  :ensure t
  :defer 1
  :config
  (global-flycheck-mode)

  (setq flycheck-display-errors-delay 0.2)
  (setq eldoc-idle-delay 0.1)

  (add-hook 'flycheck-mode-hook 'add-node-modules-path)
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  ;; (setq flycheck-eslint-args "--cache \*\*/\_.ts")
  (setq flycheck-eslint-args "--cache")
  ;; (flycheck-add-mode 'javascript-eslint 'vue-mode) ;; I don't use vue anymore
  (flycheck-add-mode 'javascript-eslint 'flow-minor-mode)
  (flycheck-add-mode 'html-tidy 'web-mode)

  (setq flycheck-checker-error-threshold 2000)

  (setq flycheck-html-tidy-executable "/usr/local/Cellar/tidy-html5/5.8.0/bin/tidy")
  (setq flycheck-protoc-import-path (list "/opt/src/predictive_maintenance/hydro/statkraft/predictive_maintenance/hydro/grpc/proto"))
  )


(defconst my-protobuf-style
  '((c-basic-offset . 2)
    (indent-tabs-mode . nil)))


(use-package protobuf-mode
  :ensure t
  :config
  (add-hook 'protobuf-mode-hook
            (lambda () (c-add-style "my-style" my-protobuf-style t))))

(use-package flycheck-color-mode-line
  :ensure t
  :hook (flycheck-mode . flycheck-color-mode-line-mode))

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
  (revert-buffer t t))

(defun eslint-fix-file-and-revert ()
  "Run eslint on current buffer."
  (interactive)
  (eslint-fix-file)
  (revert-buffer t t))

;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
;; More at http://www.emacswiki.org/emacs/ParEdit
(use-package paredit
  :disabled
  :defer 2
  :ensure t
  :config
  (add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook             #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook           #'enable-paredit-mode)

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

(use-package guru-mode
  :ensure t
  :config
  (setq guru-warn-only t)

  (guru-global-mode +1))

(use-package paredit-everywhere
  :disabled
  :defer 2
  :ensure t
  :hook (prog-mode . paredit-everywhere-mode))

(use-package prolog-mode
  :disabled
  :ensure t
  :mode "\\.pl$")

(use-package restclient
  :ensure t
  :mode "\\.http$\\'")

(use-package company-restclient
  :ensure t
  :after restclient
  :config
  (add-to-list 'company-backends 'company-restclient))

;; https://github.com/wasamasa/eyebrowse
(use-package eyebrowse
  :defer 5
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
  ;; (setq eyebrowse-post-window-switch-hook 'neo-global--attach)
  (setq eyebrowse-new-workspace t))

;; https://www.emacswiki.org/emacs/WindMove
(use-package windmove
  :ensure t
  :bind (("<S-left>" . 'windmove-left)
         ("<S-right>" . 'windmove-right)
         ("<S-up>" . 'windmove-up)
         ("<S-down>" . 'windmove-down)))

(add-to-list 'load-path "~/code/emacs-libvterm")
(use-package vterm
  :ensure t
  :config
  (setq vterm-shell "/bin/zsh")
  (add-hook 'vterm-mode-hook (lambda () (setq-local global-hl-line-mode nil)))
  (define-key vterm-mode-map (kbd "C-q") #'vterm-send-next-key)
  )

(use-package vterm-toggle
  :ensure t
  :config
  (global-set-key [f2] 'vterm-toggle)
  (global-set-key (kbd "C-<escape>") 'vterm-toggle)
  (define-key vterm-mode-map  [f2] 'vterm-toggle))

(use-package indium
  :ensure t)

(use-package fennel-mode
  :disabled
  :mode "\\.fnl$'"
  :ensure t)

;; http://www.emacswiki.org/emacs/SavePlace
(use-package saveplace
  :defer 1
  :ensure t
  :config
  (setq-default save-place t)
  (setq save-place-file (concat user-emacs-directory "places")))

;; https://github.com/hrehfeld/emacs-smart-hungry-delete
(use-package smart-hungry-delete
  :ensure t
  :bind (([remap backward-delete-char-untabify] . smart-hungry-delete-backward-char)
	       ([remap delete-backward-char] . smart-hungry-delete-backward-char)
	       ([remap delete-char] . smart-hungry-delete-forward-char))
  :init (smart-hungry-delete-add-default-hooks))

(use-package anzu
  :ensure t
  :bind (("C-1" . anzu-query-replace)
         ("C-!" . anzu-query-replace-regexp))
  :config
  (global-anzu-mode 1))

(use-package projectile
  :ensure t
  :bind (:map projectile-mode-map
              (("M-p" . 'projectile-command-map)
               ("C-c p" . 'projectile-command-map)))
  :config
  (setq projectile-project-compilation-cmd ""
        projectile-completion-system 'ivy
        projectile-enable-caching nil)
  )


(use-package projectile-ripgrep
  :after projectile
  :defer 5
  :ensure t)

;; https://github.com/dajva/rg.el
(use-package rg
  :defer 5
  :ensure t
  :ensure-system-package (rg . ripgrep)
  :config
  (rg-enable-default-bindings)
  (setq rg-executable "/usr/bin/rg"))

(use-package neotree
  :ensure t
  :bind ("C-x t" . neotree-toggle)
  :config
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

;; https://github.com/Fanael/rainbow-delimiters
(use-package rainbow-delimiters
  :ensure t
  :hook (prog-mode . rainbow-delimiters-mode))

(use-package geiser
  :commands (run-geiser)
  :ensure t)

(use-package ac-geiser
  :after geiser
  :ensure t)

;; https://github.com/TeMPOraL/nyan-mode
(use-package nyan-mode
  :ensure t
  :config
  (setq nyan-wavy-trail 1)
  (nyan-mode))

(use-package undo-tree
  :disabled
  :ensure t
  :init
  (global-undo-tree-mode)
  :config
  (undo-tree))

(use-package smerge-mode
  :config
  (setq smerge-command-prefix (kbd "C-c *")))

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-list-repositories)
         ("C-x C-S-B" . magit-blame-addition)
         ("s-x l" . magit-log-buffer-file)
         ("s-x b" . magit-blame))
  :config
  (setq magit-repository-directories
        `(("~/code" . 1)
          (,user-emacs-directory . 0)))
  (setq magit-list-refs-sortby "-creatordate"))

;(use-package forge
;  :ensure t
;  :after magit
;  :config
;  (setq forge-topic-list-order '(updated . string>))
;  )

;; https://github.com/alphapapa/magit-todos#installation
;; (use-package magit-todos
;;   :ensure t
;;   :defer 10
;;   :config
;;   (magit-todos-mode t))

;; https://github.com/syohex/emacs-git-messenger
(use-package git-messenger
  :ensure t
  :bind ("C-c M" . git-messenger:popup-message)
  :config
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

;; https://github.com/emacsorphanage/git-gutter
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 1))

(use-package ob-ipython
  :disabled
  :after company
  :ensure t
  :config
  (add-hook 'ob-ipython-mode-hook
            (lambda () (company-mode 1))))

;; https://github.com/zweifisch/ob-http
;; org mode source block http
;; (use-package ob-http
;;   :ensure t)

;; https://github.com/alphapapa/org-super-agenda/#installation
(use-package org-super-agenda
  :ensure t
  :after org
  :config
  (org-super-agenda-mode t))

(electric-pair-mode 0)

(use-package org
  :defer 1
  :ensure t ;org-plus-contrib
  :pin gnu
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c ." . org-save-all-org-buffers)
         ("C-c b" . org-switchb))
  :config
  ;; (org-clock-persistence-insinuate)
  (setq org-clock-persist t)
  (add-hook 'org-mode-hook (lambda ()
                             (visual-line-mode t)
                             (auto-save-mode t)
                             (electric-pair-local-mode 0)))

  (setq org-src-fontify-natively t
        org-src-tab-acts-natively nil
        org-hide-emphasis-markers t
        org-log-done 'time
        org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0
        org-image-actual-width nil
        org-startup-indented t
        org-directory "~/org")
  (setq org-format-latex-options (plist-put org-format-latex-options :scale 1.5))

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'org-preview-latex-fragment nil 'make-it-local)))

  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
                               (calc . t)
                               (clojure . t)
                               (shell . t)
                               (latex . t)
                               (plantuml . t)
                               ;; (http . t)
                               (js . t)
                               (gnuplot . t)))

  (setq org-plantuml-exec-mode 'plantuml)
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  (setq org-refile-targets '((nil :maxlevel . 3) (org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path t)
  (setq org-outline-path-complete-in-steps nil)

  (setq org-agenda-files (list "~/code/statkraft-notater/agenda.org"))

  (setq org-super-agenda-groups nil)

  (setq org-priority-faces '((?A . (:foreground "Dark Red" :weight bold))
                             (?B . (:foreground "DeepSkyBlue4"))
                             (?C . (:foreground "OliveDrab"))))
  (setq org-agenda-start-on-weekday nil)
  (setq org-log-into-drawer t)

  ;; https://whhone.com/posts/org-mode-task-management/ insp
  ;; https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
  (setq org-agenda-custom-commands
        '(
          ("n" "Agenda all all TODOs"
           ((agenda "")
            (alltodo "")))
          ("w" "Work view"
           (
            (agenda "" ((org-agenda-overriding-header "XXXXX")
                        (org-agenda-span 'day)
                        ;; (org-agenda-use-time-grid nil)
                        (org-super-agenda-groups
                         '(
                           (:name "Top prio"
                                  :priority "A")
                           (:name "Prio B"
                                  :priority "B")
                           (:time-grid t)
                           (:auto-outline-path t)
                           ))
                        ))
            (alltodo "" ((org-super-agenda-groups
                          '(
                            (:name "Doing"
                                   :todo "DOING")
                            (:name "Top prio"
                                   :priority "A")
                            (:name "Prio B"
                                   :priority "B")
                            (:auto-group t)
                         ))
                         (org-agenda-skip-function '(org-agenda-skip-if nil '(deadline)))
                         (org-agenda-overriding-header "ALL NONSCHEDULED TASKS")
                      )))
           ((org-agenda-files '("~/code/statkraft-notater/agenda.org"))
            (org-agenda-span 'day)
            (org-agenda-compact-blocks t))
           )
          ("I" "Import diary from iCal" agenda ""
         ((org-agenda-mode-hook
           (lambda ()
             (org-mac-iCal)))))
          ))

  (setq org-default-notes-file "~/code/statkraft-notater/agenda.org")

  ;; org capture
  (setq org-capture-templates
        '(
          ("a" "PROMO" entry
           (file+headline "~/code/statkraft-notater/agenda.org" "Usorterte todos")
           "* TODO [#C] %?\n")
          ("i" "Ide" entry
           (file+headline "~/code/statkraft-notater/agenda.org" "Ideer")
           "* %?\n %T")
	  )
	)
  (require 'org-tempo)
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit t)
  (setq org-agenda-include-diary t)
  )

(use-package ox-md
  :ensure nil
  :defer 3
  :after org)

(use-package org-bullets
  :ensure t
  :hook (org-mode . org-bullets-mode))

;; (use-package org-superstar
;;   :ensure t
;;   :hook (org-mode . org-superstar-mode))

;; In order for org mode / gnuplot to work
(use-package gnuplot
  :defer 2
  :ensure t)


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
  :disabled
  :bind
  (:map global-map
         ;; ("M-0"       . treemacs-select-window)
        ("C-z 1"   . treemacs-delete-other-windows)
        ("C-z z z"   . treemacs)
        ("C-z z B"   . treemacs-bookmark)
        ("C-z z C-t" . treemacs-find-file)
        ("C-z t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
  :disabled
  :after treemacs projectile
  :ensure t)

(use-package treemacs-icons-dired
  :disabled
  :after treemacs dired
  :ensure t
  :config (treemacs-icons-dired-mode))

(use-package treemacs-magit
  :disabled
  :after treemacs magit
  :ensure t)


(global-set-key (kbd "C-c æ")
                (lambda () (interactive) (find-file "~/code/statkraft-notater/agenda.org")))

(global-set-key (kbd "C-c i")
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))


;; https://github.com/tarsius/hl-todo
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode t))

;; https://github.com/mickeynp/ligature.el
(use-package ligature
  :disabled
  :load-path "~/.emacs.d/ligature.el"
  :config
    (ligature-set-ligatures 'prog-mode '("-|" "-~" "---" "-<<" "-<" "--" "->" "->>" "-->" "///" "/=" "/=="
                                      "/>" "//" "/*" "*>" "***" "*/" "<-" "<<-" "<=>" "<=" "<|" "<||"
                                      "<|||" "<|>" "<:" "<>" "<-<" "<<<" "<==" "<<=" "<=<" "<==>" "<-|"
                                      "<<" "<~>" "<=|" "<~~" "<~" "<$>" "<$" "<+>" "<+" "</>" "</" "<*"
                                      "<*>" "<->" "<!--" ":>" ":<" ":::" "::" ":?" ":?>" ":=" "::=" "=>>"
                                      "==>" "=/=" "=!=" "=>" "===" "=:=" "==" "!==" "!!" "!=" ">]" ">:"
                                      ">>-" ">>=" ">=>" ">>>" ">-" ">=" "&&&" "&&" "|||>" "||>" "|>" "|]"
                                      "|}" "|=>" "|->" "|=" "||-" "|-" "||=" "||" ".." ".?" ".=" ".-" "..<"
                                      "..." "+++" "+>" "++" "[||]" "[<" "[|" "{|" "??" "?." "?=" "?:" "##"
                                      "###" "####" "#[" "#{" "#=" "#!" "#:" "#_(" "#_" "#?" "#(" ";;" "_|_"
                                      "__" "~~" "~~>" "~>" "~-" "~@" "$>" "^=" "]#"))
    (global-ligature-mode 1))


(use-package company
  :defer
  :ensure t
  :hook (after-init . global-company-mode)
  :bind ("C-<tab>" . company-complete)
  :custom (company-dabbrev-downcase nil)
  :config
  ;; (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)
  ;; Don't set this to 0 if you want yasnippet to work well.
  (setq company-idle-delay 0.1)
  (setq company-show-quick-access t)
  (setq company-minimum-prefix-length 1))

(use-package pos-tip
  :ensure t)

;; https://github.com/company-mode/company-quickhelp
(use-package company-quickhelp
  :after company pos-tip
  :ensure t
  :config
  (company-quickhelp-mode))

;; https://github.com/sebastiencs/company-box
(use-package company-box
  :ensure t
  :hook (company-mode . company-box-mode))

(use-package json-mode
  :mode "\\.json\\'"
  :ensure t)

(use-package prettier-js
  :defer 2
  :bind ("C-c f" . prettier-js)
  :ensure t)

(use-package web-mode
  :ensure t
  :after (add-node-modules-path)
  :mode ("\\.[t|j]sx?$" "\\.tsx?$\\'" "\\.html?\\'")
  ;; :bind ("C-c r" . 'tide-rename-symbol-at-location)
  :config
  (setq web-mode-indentation-params '(("lineup-calls" . 1)))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?$\\'")))
  (setq web-mode-enable-auto-indentation nil)

  (add-hook 'web-mode-hook
            (lambda ()
              (flycheck-mode 1)
              (yas-activate-extra-mode 'js2-mode)
              (when (and (or (locate-dominating-file default-directory ".prettier.rc")
                             (locate-dominating-file default-directory ".prettierrc.json")
                             (locate-dominating-file default-directory ".prettierrc")))
                ;; (string= (file-name-extension buffer-file-name) "ts")
                (prettier-js-mode 1)
                (add-hook 'before-save-hook 'prettier-js nil t)
                ;; (add-hook 'before-save-hook 'tide-format-before-save)
                )
              (when (locate-dominating-file default-directory ".eslintrc.js")
                (flycheck-add-mode 'javascript-eslint 'web-mode)
                )
              (electric-indent-mode nil)
              (subword-mode +1)
              (setq web-mode-markup-indent-offset 2)
              (setq web-mode-code-indent-offset 2)
              ;; (add-hook 'after-save-hook #'eslint-fix-file-and-revert)
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode)
                (flycheck-add-next-checker 'tsx-tide 'javascript-eslint)
                )
              (when (string-equal "ts" (file-name-extension buffer-file-name))
                (setup-tide-mode)
                ;; (flycheck-add-next-checker 'javascript-eslint 'tsx-tide)
                )
              (electric-pair-local-mode t)))
  (setq web-mode-enable-auto-quoting nil))

(use-package jest
  :disabled
  :ensure t)

(use-package typescript-mode
  :defer 1
  :ensure t)

(use-package tide
  :bind (("C-c r" . tide-rename-symbol)
         ("C-c d" . tide-documentation-at-point)
         ("C-c x" . tide-refactor)
         ("C-c u" . tide-references)
         )
  ;; :hook (web-mode . my-tide-mode-hook)
  :defer 10
  :after (typescript-mode company flycheck)
  :commands setup-tide-mode
  :ensure t
  :config
  (defun setup-tide-mode ()
    (tide-setup)
    (flycheck-add-mode 'typescript-tide 'web-mode)
    (flycheck-add-next-checker 'tsx-tide 'javascript-eslint)
    (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
    (tide-hl-identifier-mode +1)
    ))

(use-package helm-system-packages
  :ensure t)

(use-package plantuml-mode
  :defer 1
  :ensure t
  :mode "\\.plantuml\\'"
  :config
  (setq plantuml-executable-path "/usr/bin/plantuml")
  (setq plantuml-output-type "png")
  (setq plantuml-default-exec-mode 'executable))

;; https://github.com/alexmurray/flycheck-plantuml
(use-package flycheck-plantuml
  :ensure t
  :after (plantuml-mode flycheck)
  :config
  (flycheck-plantuml-setup))

;; https://github.com/k1LoW/emacs-ansible
(use-package ansible
  :ensure t)

(use-package rust-mode
  :ensure t
  :mode "\\.rs\\'"
  :config
  (add-hook 'rust-mode-hook #'lsp)
  (add-hook 'rust-mode-hook
            (lambda () (add-hook 'before-save-hook 'lsp-format-buffer nil t))))

;; https://github.com/kwrooijen/cargo.el
(use-package cargo
  :ensure t
  :hook ((rust-mode . cargo-minor-mode)))

(use-package vue-mode
  :disabled
  :ensure t
  :mode "\\.vue\\'"
  :config
  (setq mmm-typescript-mode-submode-hook #'setup-vue-with-ts)
  (set-face-background 'mmm-default-submode-face nil))

(setq css-indent-offset 2)

(use-package smartparens
  :ensure t
  :defer 1
  :hook ((prog-mode . smartparens-mode)
         (elisp-mode . smartparens-strict-mode))
  :bind (:map smartparens-mode-map
              ("C-M-f" . 'sp-forward-sexp)
              ("C-M-b" . 'sp-backward-sexp)
              ("C-M-d" . 'sp-down-sexp)
              ("C-M-a" . 'sp-backward-down-sexp)
              ("C-S-d" . 'sp-beginning-of-sexp)
              ("C-S-a" . 'sp-end-of-sexp)
              ("C-M-k" . 'sp-kill-sexp)
              ;; ("S-M-<right>" . 'sp-forward-slurp-sexp)
              ;; ("S-M-<left>" . 'sp-forward-barf-sexp)
              ("C-<right>" . 'sp-forward-slurp-sexp)
              ("C-<left>" . 'sp-forward-barf-sexp)
              ("C-S-<left>" . 'sp-backward-slurp-sexp)
              ("C-S-<right>" . 'sp-backward-barf-sexp)
              )
  :config
  (require 'smartparens-config)
  (smartparens-global-mode t))


;; https://elpa.gnu.org/packages/sml-mode.html
(use-package sml-mode
  :disabled
  :mode "\\.\\(sml\\|sig\\)\\'"
  :ensure t)

;; https://github.com/codesuki/add-node-modules-path
(use-package add-node-modules-path
  :defer 1
  :ensure t
  )

(use-package glsl-mode
  :ensure t
  :mode "\\.glsl\\'")

;; Ruby
(use-package ruby-mode
  :mode "\\.rb\\'"
  :ensure t)

(use-package ruby-electric
  :ensure t
  :hook ruby-mode)

;; https://github.com/JoshCheek/seeing_is_believing
(use-package seeing-is-believing
  :ensure t
  :hook ruby-mode)

(use-package inf-ruby
  :ensure t
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package rbenv
  :ensure t
  :init
  (setq rbenv-installation-dir "/usr/local/bin/rbenv")
  )

;; Haskell
(use-package haskell-mode
  :mode "\\.hs'"
  :disabled
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
  :mode "\\.go\\'"
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
  :ensure t
  :mode "\\.rkt\\'"
  :hook racket-xp-mode
  :config
  (setq racket-program "/usr/local/bin/racket"))

(use-package dockerfile-mode
  :ensure t
  :mode "^Dockerfile\\'")

(use-package minimap
  :disabled
  :ensure t
  :bind ("C-x w" . minimap-mode)
  :config
  (setq minimap-window-location 'right)
  (setq minimap-automatically-delete-window nil))

;; https://github.com/Wilfred/helpful
(use-package helpful
  :ensure t
  :bind (("C-h k" . 'helpful-key)
         ("C-c C-d" . 'helpful-at-point))
  :after counsel
  :config
  (setq counsel-describe-function-function #'helpful-callable
	counsel-describe-variable-function #'helpful-variable))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md[x]?$")
  :ensure-system-package pandoc
  :config
  (setq-default fill-column 120)
  (add-hook 'markdown-mode-hook 'auto-fill-mode nil t)
  (add-hook 'markdown-mode-hook 'visual-line-mode)
  (add-hook 'markdown-mode 'pandoc-mode)
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

(use-package pandoc-mode
  :ensure t)

;; https://github.com/TobiasZawada/texfrag
(use-package texfrag
  :disabled ;; some error??
  :ensure t
  :hook (markdown-mode . textfrag-mode))

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


(use-package lsp-python-ms
  :disabled
  :ensure t
  :init (setq lsp-python-ms-auto-install-server t)
  :hook (python-mode . (lambda ()
                         (require 'lsp-python-ms)
                         (lsp))))

(use-package lsp-mode
  :defer 2
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (sh-mode . lsp)
         )
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (local-set-key (kbd "M-.") 'lsp-find-definition)
  (setq lsp-modeline-code-actions-segments '(count icon name))

  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 5 1024 1024)) ;; 5mb
  (setq gc-cons-threshold 100000000)
  (setq lsp-idle-delay 0.8)

  (setq lsp-file-watch-threshold 7000)
  (setq lsp-response-timeout 20)
  (setq lsp-enable-file-watchers t)

  (setq lsp-signature-doc-lines 5)
  (setq lsp-signature-render-documentation nil)
  (setq lsp-lens-enable t)

  ;; try??
  (setq lsp-tcp-connection-timeout 5)

  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]docs\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]site\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]cdk.out\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.aws-sam\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.pytest_cache\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]__pycache__\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]/Users/fredrikmeyer/code/work/audio_analytics_internal/preliminary_study/\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.mypy_cache\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\]\_build")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build/lib\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]cloud_infrastructure/dashboard\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]cloud_infrastructure/cdk-cleanup\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]data_exploration\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.cache\\'")

  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  ;; (add-hook 'python-mode-hook #'lsp)

  (require 'lsp-rust)
  ;; (require 'lsp-csharp)
  ;; (require 'lsp-pyright)
  ;; (require 'lsp-python-ms)

  (setq lsp-rust-server 'rust-analyzer)

  ;; To enable mypy
  ;; https://github.com/tomv564/pyls-mypy
  ;; Also had to install this from source:
  ; pip3 install git+https://github.com/tomv564/pyls-mypy.git
  ;; (lsp-register-custom-settings '(("pyls.plugins.pyls_mypy.enabled" t t)))
  ;; (lsp-register-custom-settings '(("pyls.plugins.pyls_isort.enabled" t t)))
  )


(use-package lsp-ui
  :defer 2
  :ensure t
  :config
  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-sideline-delay 0.1)
  (setq lsp-ui-doc-use-webkit nil)
  (setq lsp-ui-doc-webkit-max-width-px 1000)
  (setq lsp-ui-doc-header t)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-enhanced-markdown t)

  (setq lsp-ui-sideline-show-code-actions t)
  (setq lsp-ui-sideline-show-hover t)

  (setq lsp-ui-imenu-auto-refresh t)
  )

(use-package lsp-java
  ;; :disabled
  :ensure t
  :after lsp
  :config
  (add-hook 'java-mode-hook 'lsp))

(use-package lsp-yaml
  :after lsp
  :config
  (add-hook 'yaml-mode-hook #'lsp)
  (setq lsp-yaml-custom-tags ["!And"
                              "!If"
                              "!Not"
                               "!Equals"
                               "!Or"
                               "!FindInMap"
                               "!Base64"
                               "!Cidr"
                               "!Ref"
                               "!Sub"
                               "!GetAtt"
                               "!GetAZs"
                               "!ImportValue"
                               "!Select"
                               "!Split"
                               "!Join"]))

(use-package lsp-metals
  :disabled
  :ensure t
  :custom
  ;; Metals claims to support range formatting by default but it supports range
  ;; formatting of multiline strings only. You might want to disable it so that
  ;; emacs can use indentation provided by scala-mode.
  (lsp-metals-server-args '("-J-Dmetals.allow-multiline-string-formatting=off"))
  :hook (scala-mode . lsp))

(use-package lsp-json
  :after lsp
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)))
  (add-hook 'json-mode-hook #'lsp))

(use-package dap-mode
  ;; :disabled
  :ensure t
  :after lsp-mode
  :hook ((lsp-mode . dap-mode)
         (lsp-mode . dap-ui-mode))
  :config
  (require 'dap-python)
  ;; Maybe solves it...
  (dap-tooltip-mode -1)
  ;; (require 'dap-python)
  ; should fix it also
;  (dap-enable-mouse-support nil)
  ;; Will break tooltips: https://github.com/emacs-lsp/dap-mode/issues/314
  )


(use-package company-auctex
  :ensure t
  :after latex-mode
  :hook (latex-mode . (lambda () (company-auctex-init)))
  :config
  ;; (add-hook 'latex-mode-hook (company-auctex-init))
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
  :commands (fireplace)
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

(use-package modus-themes
  :ensure t
  :config
  (load-theme 'modus-operandi t)
  (setq modus-operandi-theme-rainbow-headings t)
  (setq modus-operandi-theme-scale-headings t)
  (setq modus-themes-syntax nil)
  (setq modus-themes-completions 'super-opinionated)
  )


(set-face-attribute 'default nil :height 100)

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
  :ensure t
  :hook (python-mode . whitespace-mode)
  :config
  (setq whitespace-line-column 120)
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
  :hook (prog-mode . yas-minor-mode))

(use-package yasnippet-snippets
  :ensure t
  :defer 10
  :after yasnippet
  :config
  (yas-load-directory yasnippet-snippets-dir))

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ("C-c C-<" . 'mc/mark-all-like-this))
  :ensure t)

(use-package elfeed
  :disabled ;; use rss.fredrikmeyer.net instead
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
          ("https://corey.tech/feed.xml" aws tech blog)
          ))
  :config
  (defun elfeed-open-maybe-in-xwidget (&optional use-generic-p)
    (interactive "P")
    (let ((browse-url-browser-function #'xwwp))
      (elfeed-show-visit use-generic-p)))
  (define-key elfeed-show-mode-map (kbd "B") 'elfeed-open-maybe-in-xwidget))

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
  :ensure t
  :mode "\\.y*ml"
  :config
  (add-hook 'yaml-mode-hook
            (lambda () (define-key yaml-mode-map (kbd "<C-return>") 'newline-and-indent)))
  (setq yaml-indent-offset 2))

(use-package julia-mode
  :mode "\\.jl'"
  :ensure t)

(use-package dired-subtree
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
  :bind ("C-x C-b" . ibuffer)
  :config
  ;; Shows a list of buffers
  (defalias 'list-buffers  'ibuffer)
  (setq ibuffer-saved-filter-groups
        (quote (("default"
                 ("dired" (mode . dired-mode))
                 ("PDF" (mode . pdf-view-mode))
                 ("python" (mode . python-mode))
                 ("markdown" (mode . markdown-mode))
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
                " " filename))))

(setq dired-listing-switches "-alh")

(use-package peep-dired
  :ensure t)

;; https://www.manueluberti.eu/emacs/2021/07/31/dictionary/
;; (use-package dictionary
;;   :bind ("C-c d" . #'dictionary-search)
;;   :config
;;   (setq dictionary-server "dict.org"))

(add-hook 'ibuffer-mode-hook
          (lambda ()
            (ibuffer-switch-to-saved-filter-groups "default")))

;; Indent region better:
(global-set-key (kbd "M-i") 'indent-region)


(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  nil " sticky" nil (set-window-dedicated-p (selected-window)
    sticky-buffer-mode))

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
  :defer 5
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


;; shell scripts
(setq-default sh-basic-offset 2)
(setq-default sh-indentation 2)

(setq sgml-quick-keys 'close)

;; eldoc-mode shows documentation in the minibuffer when writing code
;; http://www.emacswiki.org/emacs/ElDoc
(add-hook 'emacs-lisp-mode-hook 'turn-on-eldoc-mode)
(add-hook 'lisp-interaction-mode-hook 'turn-on-eldoc-mode)



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

;; don't pop up font menu
(global-set-key (kbd "s-t") (lambda () (interactive)))

;; no bell
(setq ring-bell-function 'ignore)
(setq visible-bell t)


(show-paren-mode 1)

(setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups"))))

(add-to-list 'load-path (expand-file-name "lisp" user-emacs-directory))
;; (use-package fm-common-lisp)
(use-package fm-python)
(use-package fm-swiper)
;; (use-package cfn-lint)
;; (use-package fm-clojure)

(use-package fm-buffer-move
  :bind
  (("C-c q u" . 'buf-move-up)
   ("C-c q d" . 'buf-move-down)
   ("C-c q l" . 'buf-move-left)
   ("C-c q r" . 'buf-move-right)))

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


;;; init.el ends here



  ;; Overwrite existing scss-stylelint checker to not use --syntax
  (flycheck-define-checker scss-stylelint
  "A SCSS syntax and style checker using stylelint.

See URL `http://stylelint.io/'."
  :command ("stylelint"
            (eval flycheck-stylelint-args)
;; "--syntax" "scss"
            (option-flag "--quiet" flycheck-stylelint-quiet)
            (config-file "--config" flycheck-stylelintrc))
  :standard-input t
  :error-parser flycheck-parse-stylelint
  :modes (scss-mode))


;; From https://github.com/flycheck/flycheck/issues/1974#issuecomment-1343495202
(flycheck-define-checker python-ruff
  "A Python syntax and style checker using the ruff utility.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.
See URL `http://pypi.python.org/pypi/ruff'."
  :command ("ruff"
            "--format=text"
            (eval (when buffer-file-name
                    (concat "--stdin-filename=" buffer-file-name)))
            "-")
  :standard-input t
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (seq-map #'flycheck-flake8-fix-error-level errors)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha)) (one-or-more digit)) " "
            (message (one-or-more not-newline))
            line-end))
  :modes python-mode)

(add-to-list 'flycheck-checkers 'python-ruff)

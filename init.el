;;; init.el --- My Emacs config -*- lexical-binding: t; -*-x

;; Author: Fredrik Meyer

;;; Commentary:

;; My personal messy Emacs config.

;;; Code:

;; (byte-recompile-directory (expand-file-name "~/.emacs.d/") 0)
;; (profiler-start 'cpu+mem)


(setq gc-cons-threshold 100000000)
(add-hook 'after-init-hook
          (lambda () (setq gc-cons-threshold 134217728)))


(add-to-list 'load-path "~/src/org-mode/lisp")

(require 'package)

(setq package-archives '(("melpa" . "https://melpa.org/packages/")
                         ("gnu" . "https://elpa.gnu.org/packages/")
                         ;; ("melpa-stable" . "https://stable.melpa.org/packages/")
                         ("nongnu" . "https://elpa.nongnu.org/nongnu/")
                         ;; ("org" . "https://orgmode.org/elpa/")
                         ("elpa" . "https://elpa.gnu.org/packages/")))

(package-initialize)
(unless package-archive-contents
  (package-refresh-contents))

(unless (package-installed-p 'use-package)
  (package-install 'use-package))


(eval-when-compile
  (require 'use-package))

(setq use-package-compute-statistics t)

(setq use-package-verbose nil)

(setq user-full-name "Fredrik Meyer"
      user-mail-address "hrmeyer@gmail.com")

;; Full path in title bar
(setq-default frame-title-format "Emacs %b (%f)")

(setq ns-pop-up-frames nil)
(setq locale-coding-system 'utf-8)
(prefer-coding-system 'utf-8)
(set-default-coding-systems 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-terminal-coding-system 'utf-8)
(set-keyboard-coding-system 'utf-8)
(set-selection-coding-system 'utf-8)

;; 50 mb threshold
(setq large-file-warning-threshold (* 50 1024 1024))

;; No need for ~ files when editing
(setq create-lockfiles nil
      auto-save-default nil
      ;; Startup?
      default-directory (concat (getenv "HOME") "/")
      custom-file (expand-file-name "custom.el" user-emacs-directory))

;; try not using custom-file
;; (when (file-exists-p custom-file)
;;   (load custom-file))

(unbind-key "C-x C-z") ;; unbind the very annoying suspend-frame
(unbind-key "C-z")

(defun my-package-recompile ()
  "Recompile all packages."
  (interactive)
  (byte-recompile-directory "~/.emacs.d/elpa" 0 t))

(setq mac-option-modifier nil
      mac-command-modifier 'meta
      mac-function-modifier 'super
      select-enable-clipboard t
      inhibit-startup-message t
      initial-scratch-message nil
      use-dialog-box nil
      split-width-threshold 160
      split-height-threshold 80
      sentence-end-double-space nil
      line-move-visual nil)

(custom-set-variables '(calendar-week-start-day 1))
(require 'calendar)
(setq calendar-date-style 'iso)

;; Changes all yes/no questions to y/n type
(fset 'yes-or-no-p 'y-or-n-p)
(setq-default cursor-type 'bar)

;; Don't use hard tabs
(setq-default indent-tabs-mode nil)

;; EDITING

;; Highlights matching parenthesis
(use-package paren
  :ensure nil
  :hook (after-init . show-paren-mode))

(use-package hl-line
  :ensure nil
  :hook (after-init . global-hl-line-mode))

(use-package delsel
  :ensure nil
  :hook (after-init . delete-selection-mode))


;; Show line numbers
(use-package display-line-numbers
  :ensure nil
  :hook (prog-mode . display-line-numbers-mode))

(use-package simple
  :ensure nil
  :hook (after-init . column-number-mode))

(setopt c-default-style '((java-mode . "java")
                          (awk-mode . "awk")
                          (other . "stroustrup")))

;; First try to indent the current line, and if the line
;; was already indented, then try `completion-at-point'
(setopt tab-always-indent t
        tab-first-completion nil)


;; Don't show whitespaces in minibuffer
(add-hook 'minibuffer-setup-hook
          (lambda () (setq-local show-trailing-whitespace nil)))

(setopt indicate-empty-lines 't)
(setopt auth-sources '(concat (getenv "HOME") "/.authinfo"))

(defun add-auto-mode (mode &rest patterns)
  "Add entries to `auto-mode-alist' to use `MODE' for all given file `PATTERNS'."
  (dolist (pattern patterns)
    (add-to-list 'auto-mode-alist (cons pattern mode))))


;; (add-to-list 'package-pinned-packages '(cider . "melpa-stable") t)

(setenv "TERMINFO" "/Applications/Ghostty.app/Contents/Resources/terminfo/")
;; (use-package crux
;;   :ensure t)

;; Sets up exec-path-from shell
;; https://github.com/purcell/exec-path-from-shell
(use-package exec-path-from-shell
  :ensure t
  :if (memq window-system '(mac ns x))
;;  :init
;;  (setq
;;   exec-path-from-shell-variables '("PATH" "MANPATH" "WORKON_HOME")
  ;;   exec-path-from-shell-arguments nil)
  :config
  (setopt exec-path-from-shell-arguments '("-l" "-i"))
  (dolist (var '("GOPATH" "OPENAI_API_KEY"))
    (add-to-list 'exec-path-from-shell-variables var))
  (exec-path-from-shell-initialize)
  ;; (exec-path-from-shell-copy-env "PATH")
  ;; (exec-path-from-shell-copy-env "OPENAI_API_KEY")
  )

;; From the docs: Set it to nil, if you use Control* or Proxy* options in
;;your ssh configuration. (I do)
;; (use-package tramp-sh
;;   :ensure nil
;;   :init
;;   (setq tramp-use-ssh-controlmaster-options nil))

;; https://gitlab.com/jabranham/system-packages
(use-package system-packages
  :ensure t
  :config
  (setq system-packages-use-sudo 'nil)
  (setq system-packages-package-manager 'brew))

(use-package package-utils
  :ensure t)

;; HippieExpand: M-n for to complete
;; http://www.emacswiki.org/emacs/HippieExpand
(use-package hippie-exp
  :ensure nil
  :bind ("M-n" . hippie-expand)
  :custom (hippie-expand-try-functions-list
           '(yas-hippie-try-expand
             try-expand-dabbrev
             try-expand-dabbrev-from-kill
             try-expand-dabbrev-all-buffers
             try-expand-whole-kill
             try-complete-file-name
             try-expand-line
             try-complete-lisp-symbol-partially
             try-complete-lisp-symbol)))

;; For some reason failed to install with use-package
;; Had to do M-x package-install pinentry
;; https://elpa.gnu.org/packages/pinentry.html
(use-package pinentry
  :ensure t
  :hook (after-init . pinentry-start))

(use-package ob-plantuml)
(use-package ob-clojure)
(use-package org-refile)
(use-package org-goto)

;; https://github.com/alphapapa/org-super-agenda/#installation
(use-package org-super-agenda
  :ensure t
  :after org
  :hook (after-init . org-super-agenda-mode))

(use-package htmlize :ensure t)

;; https://emacs.stackexchange.com/a/42158/20796
(use-package pixel-scroll
  :hook (after-init . pixel-scroll-precision-mode)
  :config
  ;; to not hijack arrow-up
  (setq auto-window-vscroll nil)
  )

(use-package org-clock)


(use-package org
  :defer t
  :preface
  ;; https://kitchingroup.cheme.cmu.edu/blog/2017/04/09/A-better-return-in-org-mode/
  ;; https://emacs.stackexchange.com/questions/80218/how-to-move-past-a-drawer-when-point-is-on-an-org-mode-headline
  (defun org-skip-metadata ()
    (interactive)
    (org-fold-show-entry)


    ;; (save-excursion)
    ;; (next-line)
    ;; (end-of-line)
    ;; (org-cycle)
    ;; (newline)
    (org-end-of-meta-data t)
    )

  (defun set-creation-date-heading-property ()
      (save-excursion
        (org-back-to-heading)
        (org-set-property "CREATED" (format-time-string "%Y-%m-%d %T"))))

  (defun my-org-mode-date-heading-on ()
      "Turn on heading creation date property."
      (interactive)
      (add-hook 'org-insert-heading-hook #'set-creation-date-heading-property))

  (defun my-org-mode-date-heading-off ()
    "Turn off heading creation date property."
    (interactive)
    (remove-hook 'org-insert-heading-hook #'set-creation-date-heading-property))

  (defun org-time-stamp-inactive-insert ()
    "Insert inactive timestamp at point."
    (interactive)
    (org-time-stamp-inactive '(16)))

  (defun my-org-mode-date-heading-toggle ()
    "Toggle on/off heading creation date property"
    (interactive)
    (if (memq #'set-creation-date-heading-property org-insert-heading-hook)
        (progn
          (my-org-mode-date-heading-off)
          (message "off - heading creation date"))
      (progn
        (my-org-mode-date-heading-on)
        (message "on - heading creation date"))))
  ;; :ensure org-plus-contrib
  :pin gnu
  :bind (("C-c l" . org-store-link)
         ("C-c c" . org-capture)
         ("C-c a" . org-agenda)
         ("C-c M-n" . 'org-skip-metadata)
         ("C-c C-." . org-time-stamp-inactive-insert)
         ("C-c ." . org-save-all-org-buffers)
         ("C-M-<return>" . org-insert-subheading)
         ("C-c b" . org-switchb))
  :hook ((org-mode . (lambda ()
                             (visual-line-mode t)
                             (auto-save-mode t)
                             (electric-pair-mode 0))))
  :custom
  (org-clock-persist t)
  :config

  ;; (my-org-mode-date-heading-on)
  (org-clock-persistence-insinuate)

  (setopt org-src-fontify-natively t
          org-src-tab-acts-natively nil
          org-hide-emphasis-markers t
          org-hide-emphasis-markers nil
          ;; The default value was buggy.
          org-fold-core-style 'overlays)
  (setq
        org-log-done 'time
        org-confirm-babel-evaluate nil
        org-edit-src-content-indentation 0
        org-image-actual-width nil
        org-startup-indented t
        org-startup-with-inline-images t
        org-directory "~/Dropbox/org"
        org-format-latex-options (plist-put org-format-latex-options :scale 1.5)
        org-plantuml-exec-mode 'plantuml
        org-preview-latex-default-process 'dvipng
        org-ellipsis " ↕"
        org-plantuml-jar-path "/opt/homebrew/bin/plantuml"
        org-babel-clojure-backend 'cider
        org-use-speed-commands t
        org-agenda-sticky nil)

  (setq org-special-ctrl-a/e t)

  ;; https://emacs.stackexchange.com/a/80170/20796
  ;; change latex commands to use absolute path instead of relative (%f -> %F)
  (let ((png (cdr (assoc 'dvipng org-preview-latex-process-alist))))
    (plist-put png :latex-compiler '("latex -interaction nonstopmode -output-directory %o %F"))
    (plist-put png :image-converter '("dvipng -D %D -T tight -o %O %F"))
    (plist-put png :transparent-image-converter '("dvipng -D %D -T tight -bg Transparent -o %O %F")))

  (add-to-list 'org-export-backends 'md)

  (add-hook 'org-mode-hook
            (lambda ()
              (add-hook 'after-save-hook 'org-preview-latex-fragment nil 'make-it-local)))
  (add-to-list 'org-src-lang-modes '("plantuml" . plantuml))

  (org-babel-do-load-languages
   'org-babel-load-languages '((python . t)
                               (calc . t)
                               (dot . t)
                               (clojure . t)
                               (shell . t)
                               (latex . t)
                               (java . t)
                               (plantuml . t)
                               (http . t)
                               (js . t)
                               (R .t)
                               (haskell . t)
                               (gnuplot . t)))
  (add-hook 'org-babel-after-execute-hook 'org-display-inline-images 'append)

  (setq org-refile-targets '((nil :maxlevel . 3)
                             (org-agenda-files :maxlevel . 9)))
  (setq org-refile-use-outline-path t)

  (setq org-outline-path-complete-in-steps nil)

  (setq org-agenda-files (list "~/Dropbox/org/nav.org"
                               "~/Dropbox/org/tasks.org"
                               "~/Dropbox/org/notater.org"))

  (setq org-super-agenda-groups nil)

  (setq org-priority-faces '((?A . (:foreground "Dark Red" :weight bold))
                             (?B . (:foreground "DeepSkyBlue4"))
                             (?C . (:foreground "OliveDrab"))))
  (setq org-agenda-start-on-weekday nil)
  (setq org-log-into-drawer t)
  (setq org-deadline-warning-days 8)
  (setq org-goto-interface 'outline-path-completion)
  (setq org-goto-max-level 7)
  ;; Fullscreen org-agenda view
  (setq org-agenda-window-setup 'only-window)

  (setq org-todo-keywords '((sequence "TODO" "DOING" "DONE")))

  ;; https://whhone.com/posts/org-mode-task-management/ insp
  ;; https://orgmode.org/worg/org-tutorials/org-custom-agenda-commands.html
  (setq org-agenda-custom-commands
        '(
          ("n" "Agenda all all TODOs"
           ((agenda "")
            (alltodo "")))
          ("p" "Private view"
           (
            (agenda "" ((org-super-agenda-groups
                         '(
                           (:log t)
                           (:name "Agenda"
                                  :time-grid t)
                           (:name "Top prio"
                                  :priority "A")
                           (:name "Next prio"
                                  :priority<= "B")
                           (:auto-outline-path t)
                           ))
                        ))
            (alltodo ""
                     ((org-super-agenda-groups
                       '(
                         (:name "Top prio todo"
                                :priority "A")
                         (:todo "REPEAT" :order 8)
                         (:todo "SOMEDAY" :order 8)
                         (:todo ("DOING")
                                :name "Doing")
                         (:auto-outline-path t)
                         ))
                      ;;(org-agenda-skip-function '(org-agenda-skip-if nil '(deadline)))
                      (org-agenda-overriding-header "All tasks:")
                      )
                  ))
           ((org-agenda-files '("~/Dropbox/org/notater.org" "~/Dropbox/org/tasks.org"))
            (org-agenda-span 'day)
            (org-agenda-show-log t)
            ;; (org-agenda-compact-blocks t)
            ))
          ("w" "Jobb - NAV"
           (
            (agenda "x"
                    ((org-super-agenda-groups
                         '(
                           (:log t)
                           (:name "Agenda"
                                 :time-grid t)
                           (:auto-outline-path t)
                           ))
                     ))
            (alltodo "xx"
                     ((org-super-agenda-groups '(
                                                 (:todo ("DOING")
                                                        :name "Doing")
                                                 (:name "Top prio todo"
                                                        :priority "A")
                                                 (:name "Statistikk"
                                                        :tag "statistikk")

                                                 (:auto-outline-path t))))))
           ((org-agenda-files '("~/Dropbox/org/nav.org"))
            ;; (org-agenda-show-log t)
            ))
          ("l" "Long view"
           ((agenda "" ((org-super-agenda-groups
                         '((:time-grid t))))
                    ))
           ((org-agenda-files '("~/Dropbox/org/notater.org" "~/Dropbox/org/tasks.org"))
            (org-agenda-span 'month)
            (org-agenda-show-log t))
           )
          ))

  (setq org-default-notes-file "~/Dropbox/org/daglige_notater.org")

  ;; org capture
  (setq org-capture-templates
        '(("n" "Note" entry
           (file+headline "~/Dropbox/org/notater.org" "Notes")
           "* %?")
          ("C" "Tanke" entry (file+headline "~/Dropbox/org/notater.org" "Tanker") "* %?\n%U" :empty-lines-after 1)
          ("d" "Dagbok" entry (file+headline "~/Dropbox/org/dagbok.org" "2024")  "** %<%Y-%m-%d>\n%?" :prepend t)
          ("g" "Ting å gjøre" entry (file+headline "~/Dropbox/org/notater.org" "Ting å jobbe på")
           "* TODO [#B] %?" :empty-lines-after 1)
          ("c" "Privat todo" entry (file+headline "~/Dropbox/org/notater.org" "Planlegging")
           "* TODO %?\n%U")
          ("w" "Jobb - NAV" entry (file+headline "~/Dropbox/org/nav.org" "Planlegging")
           "* TODO %?\n%U")
          ("r" "Log ritalin" table-line
           (file+headline "~/Dropbox/org/notater.org" "Ritalin 💊")
           "| # | %<%Y-%m-%d> |  %<%Y-%m-%d %R> | %?")
          ("l" "Å lese" entry (file+olp "~/Dropbox/org/notater.org" "Leseliste")
           "* %?")
          ("t" "Todo" entry (file "~/Dropbox/org/tasks.org")
           "** TODO %?\n%U" :empty-lines 1)))


  (require 'org-tempo)
  (require 'ox-md)
  (require 'ox-extra)
  (require 'org-habit)
  (add-to-list 'org-modules 'org-habit t)
  (setq org-agenda-include-diary t)
  (setq diary-file "~/.emacs.d/diary.google")
  )

(use-package holidays
  :custom
  (holiday-bahai-holidays nil)
  (holiday-islamic-holidays nil)
  (holiday-hebrew-holidays nil)
  (holiday-oriental-holidays nil)
  (holiday-christian-holidays nil))

(use-package sqlite-mode
  :config
  ;; from here https://christiantietze.de/posts/2024/01/emacs-sqlite-mode-open-sqlite-files-automatically/
  (defun ct/sqlite-view-file-magically ()
    "Runs `sqlite-mode-open-file' on the file name visited by the
current buffer, killing it."
    (require 'sqlite-mode)
    (let ((file-name buffer-file-name))
      (kill-current-buffer)
      (let ((default-directory (file-name-directory file-name)))
        (sqlite-mode-open-file file-name))))

  (add-to-list 'magic-mode-alist '("SQLite format 3\x00" . ct/sqlite-view-file-magically)))

;; https://github.com/tbanel/orgaggregate#dates
(use-package orgtbl-aggregate
  :ensure t)

(use-package shell-maker
  :ensure t)

(use-package acp
  :vc (:url "https://github.com/xenodium/acp.el"))

(use-package agent-shell
  :vc (:url "https://github.com/xenodium/agent-shell"))


(use-package org-ai
  :ensure ;TODO:
  :commands (org-ai-mode
             org-ai-global-mode)
  :custom
  (org-ai-default-chat-model "gpt-4")
  (org-ai-use-auth-source t)
  :init
  (add-hook 'org-mode-hook #'org-ai-mode) ; enable org-ai in org-mode
  (org-ai-global-mode) ; installs global keybindings on C-c M-a
  :config
  ;; If you are using yasnippet and want `ai` snippets
  (org-ai-install-yasnippets)
  (setopt org-ai-openai-api-token (getenv "OPENAI_API_KEY"))
  )


;; https://github.com/rejeep/prodigy.el
(use-package prodigy
  :ensure t
  :config
  (prodigy-define-service
    :name "Jekyll serve"
    :command "bundle"
    :args '("exec" "jekyll" "serve" "-l")
    :cwd "~/code/FredrikMeyer.github.io/"
    :port 4000
    )
  )

(use-package buffer-box
  :ensure t
  :vc (:url "https://github.com/rougier/buffer-box"
            :branch "master")
  )

(use-package gptel
  :ensure t
  :vc (:url "https://github.com/karthink/gptel"
            :branch master)
  :config
  (setopt gptel-api-key '(lambda () (getenv "OPENAI_API_KEY"))))

(use-package mcp
  :ensure t
  :after gptel
  :config (require 'mcp-hub)
  :hook (after-init . mcp-hub-start-all-server))


;; https://github.com/emacsorphanage/popwin
;; popwin is a popup window manager for Emacs which makes you free from the hell of
;; annoying buffers such like *Help*, *Completions*, *compilation*, and etc.
(use-package popwin
  :ensure t
  :hook (after-init . popwin-mode))

;; https://github.com/larstvei/Try
(use-package try
  :commands try
  :ensure t)

(use-package dumb-jump
  :ensure t
  :hook ('xref-backend-functions . #'dumb-jump-xref-activate)
  :custom
  (xref-show-definitions-function #'xref-show-definitions-completing-read))

(use-package which-key
  :ensure t
  :hook (after-init . which-key-mode)
  :config
  (which-key-setup-side-window-bottom))

;; https://github.com/magnars/expand-region.le
(use-package expand-region
  :ensure t
  :bind ("C-=" . 'er/expand-region))

;; https://github.com/leoliu/easy-kill
;; Use M-w to mark a region and paste to kill ring. After mark, +-d?
;; works to expand/contract region.
(use-package easy-kill
  :ensure t
  :config
  (global-set-key [remap kill-ring-save] #'easy-kill)
  (global-set-key [remap mark-sexp] #'easy-mark))

;; R
(use-package ess
  :ensure t
  :mode ("\\.r\\'" . ess-mode))

(use-package flycheck
  :ensure t
  :init
  (setq flycheck-display-errors-delay 0.2)
  (setq eldoc-idle-delay 0.1)
  :config
  (global-flycheck-mode)

  (add-hook 'flycheck-mode-hook 'add-node-modules-path)
  (flycheck-add-mode 'javascript-eslint 'web-mode)

  (setq flycheck-eslint-args "--cache \*\*/\_.tsx?")
  (setq flycheck-html-tidy-executable "/usr/local/Cellar/tidy-html5/5.8.0/bin/tidy")
  (setq flycheck-checker-error-threshold 2000)
  )

;; (use-package flycheck-posframe
;;   :ensure t
;;   :hook (flycheck-mode . flycheck-posframe-mode))

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


(use-package restclient
  :ensure t
  :mode ("\\.http\\'" . restclient-mode))

(use-package company-restclient
  :ensure t
  :after restclient
  :config
  (add-to-list 'company-backends 'company-restclient))


(defun switch-tab (n)
  "Switch to tab number N, or create a new one if it does not exist."
  (if (nth (- n 1) (tab-bar-tabs))
      (tab-select n)
    (tab-bar-new-tab-to n)))


(use-package tab-bar
  :ensure t
  :bind (
         ("M-0" . 'tab-bar-switch-to-tab)
         ("M-1" . (lambda () (interactive ) (switch-tab 1)))
         ("M-2" . (lambda () (interactive ) (switch-tab 2)))
         ("M-3" . (lambda () (interactive ) (switch-tab 3)))
         ("M-4" . (lambda () (interactive ) (switch-tab 4)))
         ("C-'" . 'tab-next)
         ("C-§" . 'tab-previous)
         ("s-!" . 'tab-bar-rename-tab)
              )
  :init
  (dotimes (n 4)
    (global-unset-key (kbd (format "C-%d" n)))
    (global-unset-key (kbd (format "M-%d" n))))
  :config
  (tab-bar-mode t))


;; https://www.emacswiki.org/emacs/WindMove
(use-package windmove
  :ensure t
  :bind (("<S-left>" . 'windmove-left)
         ("<S-right>" . 'windmove-right)
         ("<S-up>" . 'windmove-up)
         ("<S-down>" . 'windmove-down)))

(use-package buffer-move
  :ensure t
  :bind
  (("C-c q u" . 'buf-move-up)
   ("C-c q d" . 'buf-move-down)
   ("C-c q l" . 'buf-move-left)
   ("C-c q r" . 'buf-move-right)))


;; http://www.emacswiki.org/emacs/SavePlace
(setq-default save-place t)
(setq save-place-file (concat user-emacs-directory "places"))

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
  :hook (after-init . global-anzu-mode))

;; https://docs.projectile.mx/projectile/index.html
(use-package projectile
  :ensure t
  :ensure-system-package fd
  :bind (:map projectile-mode-map
              (("M-p" . 'projectile-command-map)
               ("C-c p" . 'projectile-command-map)))
  :config
  (setq projectile-project-compilation-cmd ""
        projectile-completion-system 'ivy
        projectile-enable-caching nil))

;; https://github.com/dajva/rg.el
(use-package rg
  :defer t
  :ensure t
  :ensure-system-package (rg . ripgrep)
  :config
  (rg-enable-default-bindings)
  (setq rg-executable "/opt/homebrew/bin/rg"))

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
  :ensure t)

(use-package prog-mode
  :ensure nil
  :init
  (defun show-trailing-whitespace-local ()
    (setq-local show-trailing-whitespace t))
  :hook ((prog-mode . rainbow-delimiters-mode)
         (prog-mode . show-trailing-whitespace-local)))

(use-package geiser
  :commands (geiser)
  :ensure t)

(use-package geiser-racket
  :after geiser
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

(use-package lean4-mode
  :commands lean4-mode
  :vc (:url "https://github.com/leanprover-community/lean4-mode.git"
       :rev :last-release
       ;; Or, if you prefer the bleeding edge version of Lean4-Mode:
       ;; :rev :newest
       ))

;; Veldig vanskelig å bruke?
(use-package undo-tree
  :disabled
  :ensure t
  :init
  (global-undo-tree-mode)
  :config
  ;; Try reset original map
  (define-key undo-tree-map (kbd "C-x u") nil))

;; https://github.com/misohena/el-easydraw
;; [​[edraw:file=./example.edraw.svg]​]
;; [​[edraw:data=<base64data>​]]
;; [​[*Example][edraw:file=./example.edraw.svg]​]
;; [​[*Example][edraw:data=<base64data>]​]
(use-package edraw-org
  :ensure t
  :after org
  :vc (:url "https://github.com/misohena/el-easydraw"
            :branch master)
  :config
  (edraw-org-setup-default))


(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-list-repositories)
         ("C-x C-S-B" . magit-blame-addition))
  :init
  (setq magit-repository-directories
        `(("~/code" . 1)
          ("~/code/work" . 1)
          (,user-emacs-directory . 0)))
  (setq magit-list-refs-sortby "-creatordate"))

(use-package forge
  :ensure t
  :after magit
  :config
  (setq forge-topic-list-order '(updated . string>))
  (push '("ghe.nav.no" "ghe.nav.no/api/v3"
        "ghe.nav.no" forge-github-repository)
        forge-alist)

  )

;; https://github.com/Artawower/blamer.el
(use-package blamer
  :ensure t
  :bind (("s-j" . blamer-show-commit-info)
         ("C-c j" . blamer-show-posframe-commit-info))
  :defer 20
  :custom
  (blamer-idle-time 0.3)
  (blamer-min-offset 70)
  :custom-face
  (blamer-face ((t :foreground "#7a88cf"
                    :background nil
                    :height 120
                    :italic t)))
  :config
  (global-blamer-mode 1))

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
  :init
  (setq git-messenger:show-detail t
        git-messenger:use-magit-popup t))

;; https://github.com/emacsorphanage/git-gutter
(use-package git-gutter
  :ensure t
  :config
  (global-git-gutter-mode 1))

;; https://github.com/sshaw/git-link?tab=readme-ov-file
(use-package git-link
  :ensure t
  :bind (("C-c g l" . 'git-link)
         ("C-c g c" . 'git-link-commit)
         ("C-c g h" . 'git-link-homepage)))

;; https://codeberg.org/pidu/git-timemachine
(use-package git-timemachine
  :ensure t)


;; https://github.com/jdtsmith/speedrect
(use-package speedrect
  :load-path "~/.emacs.d/speedrect")

;; https://github.com/zweifisch/ob-http
;; org mode source block http
(use-package ob-http
  :ensure t)

;; https://emacsredux.com/blog/2023/04/11/looking-up-words-in-a-dictionary/
;; https://www.manueluberti.eu/emacs/2021/07/31/dictionary/
(use-package dictionary
  :bind ("C-x C-l" . #'dictionary-lookup-definition)
  :config
  (setq dictionary-server "dict.org"))



(use-package ox-md
  :ensure nil
  :defer t
  :after org)

;; https://github.com/snosov1/toc-org
(use-package toc-org
  :ensure t
  :hook (org-mode . toc-org-mode))

;; (use-package org-bullets
;;   :ensure t
;;   :hook (org-mode . org-bullets-mode))

(use-package org-superstar
  :ensure t
  :hook (org-mode . org-superstar-mode))

;; In order for org mode / gnuplot to work
(use-package gnuplot
  :defer t
  :ensure t)

(use-package org-roam
  :ensure t
  :after org
  :custom
  (org-roam-directory "~/Dropbox/org/roam/")
  :bind (("C-c n f" . org-roam-node-find)
         ("C-c n r" . org-roam-node-random)
         ("C-c n l" . org-roam-buffer-toggle)
         (:map org-roam-mode-map
               (("C-c n l" . org-roam)
                ("C-c n r" . org-roam-buffer-toggle-display)
                ("C-c n f" . org-roam-find-file)
                ("C-c n t" . org-roam-tag-add)
                ("C-c n g" . org-roam-graph))
               :map org-mode-map
               (("C-c n i" . org-roam-node-insert))
               (("C-c n I" . org-roam-insert-immediate))))
  :config
  (setq org-roam-completion-everywhere t)
  (org-roam-db-autosync-mode t)
  ;; (require 'org-roam-protocol)
  )


;; https://github.com/org-roam/org-roam-ui
(use-package org-roam-ui
  :ensure t
  :commands (org-roam-ui-open))

;; https://github.com/abo-abo/org-download
(use-package org-download
  :ensure t)

;; https://github.com/unhammer/org-rich-yank
(use-package org-rich-yank
  :ensure t
  :demand t
  :bind (:map org-mode-map
              ("C-M-y" . org-rich-yank))) ;; doesn't work??

;; https://github.com/IvanMalison/org-projectile
(use-package org-projectile
  :disabled
  :defer t
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
        ("M-0"       . treemacs-select-window)
        ("C-x t 1"   . treemacs-delete-other-windows)
        ("C-x t t"   . treemacs)
        ("C-x t B"   . treemacs-bookmark)
        ("C-x t C-t" . treemacs-find-file)
        ("C-x t M-t" . treemacs-find-tag)))

(use-package treemacs-projectile
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

(global-set-key (kbd "C-c o")
                (lambda () (interactive) (find-file "~/Dropbox/org/notater.org")))

(global-set-key (kbd "C-c ø")
                (lambda () (interactive) (find-file "~/Dropbox/org/okonomi.org")))

(global-set-key (kbd "C-c æ")
                (lambda () (interactive) (find-file "~/Dropbox/org/nav.org")))

(global-set-key (kbd "C-c i")
                (lambda () (interactive) (find-file "~/.emacs.d/init.el")))


;; https://github.com/tarsius/hl-todo
(use-package hl-todo
  :ensure t
  :config
  (global-hl-todo-mode t))

(use-package company
  :ensure t
  :hook (after-init . global-company-mode)
  :bind ("C-<tab>" . company-complete)
  :custom (company-dabbrev-downcase nil)
  :config
  ;; (define-key company-mode-map [remap indent-for-tab-command] #'company-indent-or-complete-common)
  ;; Don't set this to 0 if you want yasnippet to work well.
  ;; (add-to-list 'company-backends 'company-ispell)
  (setq company-idle-delay 0.1)
  (setq company-show-quick-access t)
  (setq company-minimum-prefix-length 1))

;; Not sure if I need this??
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
  :bind (("C-c C-b" . 'json-pretty-print-buffer))
  :ensure t)

(use-package prettier-js
  :defer t
  :ensure t)


(use-package web-mode
  :ensure t
  :after (add-node-modules-path)
  :mode ("\\.[t|j]sx?$" "\\.tsx?$\\'" "\\.html?\\'")
  :bind (:map web-mode-map
              ("C-c f" . prettier-js)
              ("C-c u" . tide-references))
  :config
  (setq web-mode-indentation-params '(("lineup-calls" . 2)))
  (setq web-mode-content-types-alist '(("jsx" . "\\.js[x]?$\\'")))
  (setq web-mode-enable-auto-indentation nil)
  (setq js-indent-level 2)
  (setq web-mode-markup-indent-offset 2
        web-mode-code-indent-offset 2
        web-mode-attr-indent-offset 2)

  (add-hook 'web-mode-hook
            (lambda ()
              (yas-activate-extra-mode 'js2-mode)
              (prettier-js-mode 1)
              (add-hook 'before-save-hook 'prettier-js nil t)
              (when (locate-dominating-file default-directory ".eslintrc.js")
                ;; (flycheck-add-mode 'javascript-eslint 'web-mode)
                )
              (electric-indent-mode nil)
              ;; (add-hook 'after-save-hook #'eslint-fix-file-and-revert)
              (when (string-equal "tsx" (file-name-extension buffer-file-name))
                (setup-tide-mode)
                ;; (flycheck-add-next-checker 'tsx-tide 'javascript-eslint)
                )
              (when (string-equal "ts" (file-name-extension buffer-file-name))
                (setup-tide-mode)
                ;; (flycheck-add-next-checker 'typescript-tide 'javascript-eslint)
                )

              (when (equal web-mode-content-type "html")
                ;; (flycheck-add-mode 'html-tidy 'web-mode)
                )
              ;;(electric-pair-mode t)
              ))
  (setq web-mode-enable-auto-quoting nil))

;; https://depp.brause.cc/nov.el/
(use-package nov
  :mode ("\\.epub$" . nov-mode)
  :ensure t)

(use-package jest
  :disabled
  :ensure t
  :defer t)

(use-package justl
  :ensure t
  :custom
  (justl-executable "/opt/homebrew/bin/just"))

(use-package typescript-mode
  :ensure t)

(use-package tide
  :bind (("C-c r" . tide-rename-symbol)
         ("C-c C-p" . tide-references))
  :after (typescript-mode company flycheck)
  :commands setup-tide-mode
  :ensure t
  :config
  (setq tide-always-show-documentation t
        tide-completion-detailed t)
  (flycheck-add-next-checker 'tsx-tide 'javascript-eslint 'append)
  (flycheck-add-next-checker 'javascript-eslint 'javascript-tide 'append)
  (flycheck-add-mode 'javascript-tide 'web-mode)
  (defun setup-tide-mode ()
    (tide-setup)
    ;; (flycheck-add-mode 'typescript-tide 'web-mode)
    (tide-hl-identifier-mode +1)
    ))

(use-package helm-system-packages
  :disabled
  :ensure t)

(use-package plantuml-mode
  :ensure t
  :mode ("\\.plantuml\\'" "\\.puml\\'")
  :bind (("C-c C-c" . plantuml-preview))
  :config
  (setq plantuml-executable-path "/opt/homebrew/bin/plantuml")
  (setq plantuml-output-type "png")
  (setq plantuml-jar-path "/opt/homebrew/Cellar/plantuml/1.2025.2/libexec/plantuml.jar")
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

(use-package vue-mode
  :disabled
  :ensure t
  :mode "\\.vue\\'"
  :config
  (setq mmm-typescript-mode-submode-hook #'setup-vue-with-ts)
  (set-face-background 'mmm-default-submode-face nil))


;; https://github.com/emacs-dashboard/emacs-dashboard
(use-package dashboard
  :ensure t
  :init
  (setq dashboard-set-heading-icons t)
  (setq dashboard-set-file-icons t)
  (setq dashboard-items '((recents  . 5)
                          (projects . 10)
                          ;; (bookmarks . 0)
                          (agenda . 5)))
  :config
  (dashboard-setup-startup-hook))

(use-package smartparens
  :ensure t
  :hook ((prog-mode . smartparens-mode)
         (elisp-mode . smartparens-strict-mode))
  :bind (:map smartparens-mode-map
              ("C-M-f" . 'sp-forward-sexp)
              ("C-M-b" . 'sp-backward-sexp)
              ("C-M-d" . 'sp-down-sexp)
              ("C-M-a" . 'sp-backward-down-sexp)
              ("C-S-d" . 'sp-beginning-of-sexp)
              ("C-S-a" . 'sp-end-of-sexp)
              ("C-M-t" . 'sp-transpose-sexp)
              ("C-M-k" . 'sp-kill-sexp)
              ("M-D" . 'sp-splice-sexp)
              ("C-M-w" . 'sp-copy-sexp)
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
  :ensure t
  :custom
  (add-node-modules-path-command '("echo \"$(npm root)/.bin\"")))

(use-package glsl-mode
  :ensure t
  :mode "\\.glsl\\'")

;; Ruby
(use-package ruby-mode
  :mode "\\.rb\\'"
  :ensure t)

(use-package ruby-electric
  :ensure t
  :disabled
  :hook ruby-mode)

;; https://github.com/JoshCheek/seeing_is_believing
(use-package seeing-is-believing
  :ensure t
  :disabled)

(use-package inf-ruby
  :ensure t
  :disabled
  :hook (ruby-mode . inf-ruby-minor-mode))

(use-package rbenv
  :ensure t
  :disabled
  :init
  ;; (setq rbenv-installation-dir nil)
  (setq rbenv-executable "/usr/local/bin/rbenv")
  )

(use-package robe
  :ensure t
  :disabled
  :hook (ruby-mode . robe-mode)
  :config
  (eval-after-load 'company
    '(push 'company-robe company-backends)))

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
;; https://github.com/dominikh/go-mode.el
(use-package go-mode
  :ensure t
  :mode "\\.go\\'"
  :bind (("C-c C-b" . 'gofmt))
  :config
  (if (not (string-match "go" compile-command))
      (set (make-local-variable 'compile-command)
           "go build -v && go test -v && go vet"))
  (add-hook 'go-mode-hook #'lsp-deferred)
  (add-hook 'go-mode-hook (lambda ()
                            (setq tab-width 4)))
  )

;; https://www.racket-mode.com
(use-package racket-mode
  :ensure t
  :mode "\\.rkt\\'"
  :hook racket-xp-mode)

(use-package dockerfile-mode
  :ensure t
  :mode "^Dockerfile\\'")

(use-package docker
  :ensure t
  :commands docker)

(use-package kubernetes
  :ensure t
  :commands (kubernetes-overview)
  :custom
  (kubernetes-poll-frequency 3600)
  (kubernetes-redraw-frequency 3600))


;; https://github.com/TxGVNN/emacs-k8s-mode
(use-package k8s-mode
  :ensure t
  :hook (k8s-mode . yas-minor-mode))

;; https://github.com/Wilfred/helpful
(use-package helpful
  :ensure t
  :bind (("C-h k" . 'helpful-key)
         ("C-c C-d" . 'helpful-at-point)
         ("C-h x" . 'helpful-command)
         )
  :after counsel
  :custom
  (counsel-describe-function-function #'helpful-callable)
  (counsel-describe-variable-function #'helpful-variable))

(use-package markdown-mode
  :ensure t
  :mode ("\\.md[x]?$")
;;  :ensure-system-package pandoc
  :config
  (setq-default fill-column 120)
  (add-hook 'markdown-mode-hook 'auto-fill-mode nil t)
  (add-hook 'markdown-mode-hook 'visual-line-mode)
  (setq markdown-fontify-code-blocks-natively t)
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

;; https://github.com/vedang/pdf-tools?tab=readme-ov-file#features
(use-package pdf-tools
  :ensure t
  :mode "\\.pdf\\'"
  :config
  (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  (setq pdf-view-display-size 'fit-page)
  ;; (define-key pdf-view-mode-map (kbd "C-s") 'isearch-forward)
  ;; (define-key pdf-view-mode-map (kbd "h") 'pdf-annot-add-highlight-markup-annotation)
  ;; (define-key pdf-view-mode-map (kbd "t") 'pdf-annot-add-text-annotation)
  ;; (define-key pdf-view-mode-map (kbd "D") 'pdf-annot-delete)
  ;; (setq pdf-view-use-unicode-ligther nil)
  ;; (pdf-tools-install)
  (pdf-loader-install)
  )

;; https://github.com/org-noter/org-noter?tab=readme-ov-file
(use-package org-noter
  :ensure t)

(use-package djvu :ensure t)

;; Automatically refreshes PDF
(add-hook 'doc-view-mode-hook 'auto-revert-mode)
;; (add-hook 'doc-view-mode-hook (lambda () (linum-mode 0)))
;; Requires `brew install ghostscript`

(use-package lsp-ui
  :ensure t)

(use-package lsp-mode
  :ensure t
  :init (setq lsp-keymap-prefix "C-c l")
  :hook ((lsp-mode . lsp-enable-which-key-integration)
         (sh-mode . lsp)
         (lsp-mode . lsp-completion-mode)
         (kotlin-mode . lsp)
         )
  :init
  (setq lsp-modeline-code-actions-segments '(count icon name))

  (setenv "JAVA_HOME" "/Users/fredrikmeyer/.jenv/versions/21")
  (setopt lsp-clients-kotlin-server-executable nil)

  ;; https://emacs-lsp.github.io/lsp-mode/page/performance/
  (setq read-process-output-max (* 5 1024 1024)) ;; 5mb
  (setq gc-cons-threshold 100000000)
  (setq lsp-idle-delay 0.8)

  (setq lsp-file-watch-threshold 7000)
  (setq lsp-response-timeout 20)
  (setq lsp-enable-file-watchers t)

  (setq lsp-signature-doc-lines 5)
  (setq lsp-signature-render-documentation t)
  (setq lsp-lens-enable t)

  (setq lsp-ui-doc-enable t)
  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-delay 0.1)
  (setq lsp-ui-doc-use-webkit nil)
  (setq lsp-ui-doc-webkit-max-width-px 1000)
  (setq lsp-ui-doc-header t)
  (setq lsp-lens-place-position 'above-line)
  (setq lsp-ui-doc-include-signature t)
  (setq lsp-ui-doc-enhanced-markdown t)

  (setq lsp-ui-sideline-show-code-actions nil)
  (setq lsp-ui-sideline-show-hover nil)
  (setq lsp-ui-sideline-enable nil)
  (setq lsp-ui-peek-enable t)

  ;; Todo: only for Clojure mode later
  ;; (setq lsp-completion-enable nil)

  ;; Kotlin


  ;; try??
  (setq lsp-tcp-connection-timeout 5)
  :config
  (define-key lsp-mode-map (kbd "C-c l") lsp-command-map)
  (local-set-key (kbd "M-.") 'lsp-find-definition)

  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]docs\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]site\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.pytest_cache\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]__pycache__\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.mypy_cache\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\]\_build")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]build/lib\\'")
  (add-to-list 'lsp-file-watch-ignored-directories "[/\\\\]\\.cache\\'")

  (add-hook 'lsp-mode-hook #'lsp-enable-which-key-integration)
  ;; (add-hook 'python-mode-hook #'lsp)

  (require 'lsp-rust)
  (setq lsp-rust-server 'rust-analyzer)

  (setq lsp-lens-enable 't)
  )

(use-package lsp-java
  :ensure t
  :hook (java-mode . lsp)
  :config
  (setq lsp-java-vmargs '("-XX:+UseParallelGC" "-XX:GCTimeRatio=4" "-XX:AdaptiveSizePolicyWeight=90" "-Dsun.zip.disableMemoryMapping=true"
                          "-Xmx4G" "-Xms100m"))
  (setq lsp-java-jdt-download-url "https://www.eclipse.org/downloads/download.php?file=/jdtls/milestones/1.44.0/jdt-language-server-1.44.0-202501221502.tar.gz"))

(use-package lsp-json
  :after lsp
  :config
  (add-hook 'json-mode-hook
            (lambda ()
              (make-local-variable 'js-indent-level)
              (setq js-indent-level 2)))
  (add-hook 'json-mode-hook #'lsp))

(defun kotlin-lsp-server-start-fun (port)
  (list "kotlin-lsp" "--socket" (number-to-string port)))

(with-eval-after-load 'lsp-mode
  (add-to-list 'lsp-language-id-configuration
	       '(kotlin-mode . "kotlin"))

  (lsp-register-client
   (make-lsp-client
    :new-connection (lsp-tcp-connection 'kotlin-lsp-server-start-fun)
    :activation-fn (lsp-activate-on "kotlin")
    :major-modes '(kotlin-mode)
    :priority -1
    :server-id 'kotlin-jb-lsp
    )
   )
  )

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
  :defer t
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
  (setq doom-modeline-minor-modes nil)
  (setq doom-modeline-icon t)
  (setq doom-modeline-buffer-file-name-style 'buffer-name))

;; https://github.com/johanvts/emacs-fireplace
(use-package fireplace
  :commands (fireplace)
  :ensure t)

;; https://github.com/abo-abo/ace-window
(use-package ace-window
  :ensure t
  :bind (("M-o" . ace-window)))

(use-package spacemacs-theme
  :ensure t
;  :disabled
  )

(use-package leuven-theme
  :ensure t
  :disabled
  :config
  (load-theme 'leuven t))

(use-package material-theme
  :ensure t)

(use-package modus-themes
  :ensure t
  :config
  (setq modus-themes-headings '((1 . (rainbow variable-pitch 1.1))
                                (2 . (rainbow semibold 1))
                                (t . (rainbow))
                                ))

  (load-theme 'modus-operandi t))

;; (use-package org-modern
;;   :ensure t
;;   :config
;;   (global-org-modern-mode)
;;   (add-hook 'org-mode-hook #'org-modern-mode))


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
  :custom
  (whitespace-line-column 120)
  (whitespace-style '(face lines-tail)))

;; This package implements a menu that lists enabled minor-modes, as well as
;; commonly but not currently enabled minor-modes.
;; https://github.com/tarsius/minions
(use-package minions
  :defer t
  :ensure t
  :hook (after-init . minions-mode))

;; https://github.com/joaotavora/yasnippet
(use-package yasnippet
  :ensure t
  :hook (prog-mode . yas-minor-mode)
  :hook (org-mode . yas-minor-mode)
  :init
  (add-to-list 'yas-snippet-dirs  "~/.emacs.d/snippets")
  )

(use-package ivy-yasnippet
  :ensure t
  :after yasnippet)

(use-package devdocs
  :bind ("C-h #" . 'devdocs-lookup)
  :ensure t)

(use-package yasnippet-snippets
  :ensure t
  ;; :disabled
  :after yasnippet
  :init
  ;; (yas-load-directory yasnippet-snippets-dir)
  )

;; https://github.com/magnars/multiple-cursors.el
(use-package multiple-cursors
  :bind (("C-S-c C-S-c" . 'mc/edit-lines)
         ("C->" . 'mc/mark-next-like-this)
         ("C-<" . 'mc/mark-previous-like-this)
         ;; from here https://pragmaticemacs.wordpress.com/2017/03/06/add-multiple-cursors-with-mouse-clicks/
         ("C-S-<mouse-1>". 'mc/add-cursor-on-click)
         ("C-c C-<" . 'mc/mark-all-like-this))
  :ensure t)

;; https://github.com/zk-phi/phi-search
(use-package phi-search
  :ensure t)

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
  :hook (yaml-mode . yas-minor-mode)
  :hook (yaml-mode . lsp)
  :config
  (add-hook 'yaml-mode-hook
            (lambda () (define-key yaml-mode-map (kbd "<C-return>") 'newline-and-indent)))
  (setq yaml-indent-offset 2))

(use-package dired-subtree
  :ensure t
  :after dired
  :bind (:map dired-mode-map
              ("i" . dired-subtree-insert)
              (";" . dired-subtree-remove)
              ("<tab>" . dired-subtree-toggle))
  :config
  (setq dired-subtree-use-backgrounds nil))

(use-package casual-dired
  :ensure t
  :bind (:map dired-mode-map
              ("C-x c" . 'casual-dired-tmenu)))

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
                 ("go" (mode . go-mode))
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

(use-package dired
  :custom
  ;; Found here https://emacs.stackexchange.com/a/29101/20796
  (insert-directory-program "gls")
  (dired-use-ls-dired "t")
  (dired-listing-switches "-alh")
  :config
  (when (string= system-type "darwin")
    (setq dired-use-ls-dired nil)))

(use-package peep-dired
  :ensure t)

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
  :custom
  (uniquify-buffer-name-style 'forward))

;; Turn on recent file mode so that you can more easily switch to
;; recently edited files when you first start emacs
(use-package recentf
  :ensure t
  :hook (after-init . recentf-mode)
  :custom
  (recentf-save-file (concat user-emacs-directory ".recentf"))
  (recentf-max-saved-items 50)
  (recentf-max-menu-items 200))

;; https://github.com/joaotavora/breadcrumb
(use-package breadcrumb
  :ensure t
  :hook (after-init . breadcrumb-mode))


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
(use-package fm-common-lisp)
(use-package fm-python)
(use-package fm-swiper)
;; (use-package cfn-lint)
(use-package fm-clojure)
(use-package fm-rust)
(use-package flycheck-ruff)

(define-minor-mode sticky-buffer-mode
  "Make the current window always display this buffer."
  :init-value nil :lighter "sticky" :keymap nil
  (set-window-dedicated-p (selected-window)
                          sticky-buffer-mode))

;;;; Useful functions

(defun get-current-date ()
  "Get current date as a string."
  (shell-command-to-string "echo -n $(date +%Y-%m-%d)"))

(defun insert-current-date ()
  "Insert current date."
  (interactive)
  (insert (get-current-date)))

(global-set-key (kbd "C-c d") 'insert-current-date)

(defun insert-current-date-with-weekday ()
  "Insert current date with weekday."
  (interactive)
  (insert (shell-command-to-string "echo -n $(date +'%Y-%m-%d %A')")))

(defun generate-scratch-buffer ()
  "Create and switch to a temporary scratch buffer with a random name."
  (interactive)
  (switch-to-buffer (make-temp-name "scratch-")))

(defun rerun-last-async-shell ()
  "Rerun last run shell command async."
  (interactive)
  (let ((last-command (car shell-command-history)))
    (async-shell-command last-command)))

(global-set-key (kbd "C-M-§") 'rerun-last-async-shell)

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

(global-set-key [(meta up)]   'move-line-up)
(global-set-key [(meta down)] 'move-line-down)

;; https://emacs.stackexchange.com/a/52424/20796
(defun +org-toggle-inline-image-at-point ()
  "Toggle inline image at point."
  (interactive)
  (if-let* ((bounds (and (not org-inline-image-overlays)
                         (org-in-regexp org-link-any-re nil t)))
            (beg (car bounds))
            (end (cdr bounds)))
      (org-display-inline-images nil nil beg end)
    (org-toggle-inline-images)))

(defun +org-refresh-image-at-point ()
  "Refresh image at point. If point is not on an image, refresh all images in buffer."
  (interactive)
  (+org-toggle-inline-image-at-point)
  (+org-toggle-inline-image-at-point))


(defun switch-theme (theme)
  "This interactive call is taken from `load-theme'."
  (interactive
   (list
    (intern (completing-read "Load custom theme: "
                             (mapcar 'symbol-name
                                     (custom-available-themes))))))
  (mapcar #'disable-theme custom-enabled-themes)
  (load-theme theme t))

(defun email-to-name (email)
  "Split name from EMAIL to naïvely try to generate full name."
  (let* ((f (string-split email "@"))
         (n (string-split (car f) "\\.")))
    (s-join " " (mapcar 's-capitalize n))))


(defun co-author ()
  "Insert co-author."
  (interactive)
  (let* ((email (read-string "Email: "))
         (name (email-to-name email)) )
    (insert (format "Co-authored-by: %s <%s>\n"
                    name email))))


(defun run-shell-command (command)
  "Run a shell COMMAND and display the output in a new buffer."
  (interactive "sEnter shell command: ")
  (let ((output-buffer (get-buffer-create "*Shell Command Output*")))
    (with-current-buffer output-buffer
      (erase-buffer)
      (insert (shell-command-to-string (concat "llm" " " command))))
    (display-buffer output-buffer)))

(defun set-brightness ()
  (interactive)
  (let* ((answer (read-number "Prosent ")))
    (do-applescript (format "
tell application \"Shortcuts Events\"
	run the shortcut named \"Set Brightness\" with input \"%s\"
end tell
" answer))))



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

;; DISABLED PACKAGES

(use-package use-package-ensure-system-package
  :disabled
  :ensure t)

(use-package benchmark-init
  :disabled
  :ensure t
  :config
  ;; To disable collection of benchmark data after init is done.
  (add-hook 'after-init-hook 'benchmark-init/deactivate))

;; Cheatsheet: http://www.emacswiki.org/emacs/PareditCheatsheet
;; More at http://www.emacswiki.org/emacs/ParEdit
(use-package paredit
  :disabled
  :defer t
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

(use-package paredit-everywhere
  :disabled
  :defer t
  :ensure t
  :hook (prog-mode . paredit-everywhere-mode))

(use-package vterm
  :ensure t
  :commands vterm
  :custom
  (vterm-always-compile-module t)
  :hook
  (vterm-mode . (lambda ()
                  ;; https://emacsredux.com/blog/2020/11/21/disable-global-hl-line-mode-for-specific-modes/)
                  (setq-local global-hl-line-mode nil)))
  )


(use-package claude-code-ide
  :vc (:url "https://github.com/manzaltu/claude-code-ide.el" :branch "main")
  :bind ("C-c C-'" . claude-code-ide-menu) ; Set your favorite keybinding
  :config
  (setopt claude-code-ide-cli-path "/opt/homebrew/bin/claude")
  (claude-code-ide-emacs-tools-setup))

(use-package prolog-mode
  :disabled
  :ensure t
  :mode "\\.pl$")

(use-package fennel-mode
  :disabled
  :mode "\\.fnl$'"
  :ensure t)

(use-package neotree
  :disabled
  :ensure t
  :bind ("C-x t" . neotree-toggle)
  :config
  (setq neo-smart-open t)
  (setq neo-theme 'icons)
  (setq neo-window-fixed-size nil)
  (setq neo-show-hidden-files t)
  ;; https://github.crookster.org/macOS-Emacs-26-display-line-numbers-and-me/
  (add-hook 'neo-after-create-hook (lambda (&rest _) (display-line-numbers-mode -1))))

;; Try Oz

;; (setq my-oz-home "/Applications/Mozart2.app/Contents/Resources")

;; (when (file-directory-p my-oz-home)
;;   (setenv "OZHOME" my-oz-home)
;; )

;; ;; (setq my-mozart-elisp "/usr/share/mozart/elisp")
;; (setq my-mozart-elisp "/Applications/Mozart2.app/Contents/Resources/share/mozart/elisp")

;; (when (file-directory-p my-mozart-elisp)
;;   (add-to-list 'load-path my-mozart-elisp)
;;   (load "mozart")
;;   (add-to-list 'auto-mode-alist '("\\.oz\\'" . oz-mode))
;;   (add-to-list 'auto-mode-alist '("\\.ozg\\'" . oz-gump-mode))
;;   (autoload 'run-oz "oz" "" t)
;;   (autoload 'oz-mode "oz" "" t)
;;   (autoload 'oz-gump-mode "oz" "" t)
;;   (autoload 'oz-new-buffer "oz" "" t)
;; )

;; (eval-after-load "oz-mode"
;;   '(progn
;;     (define-key oz-mode-map (kbd "C-x SPC") 'rectangle-mark-mode)
;; ))

;; https://www.reddit.com/r/emacs/comments/1mdi8vu/a_transient_for_help/
;;; A Help Transient on C-S-h
(transient-define-prefix hrm-help-transient ()
  "Help commands that I use. A subset of C-h with others thrown in."
  ["Help Commands"
   ["Mode & Bindings"
    ("m" "Mode" describe-mode)
    ;; ("M" "Minor Modes" consult-minor-mode-menu)
    ("b" "Major Bindings" which-key-show-full-major-mode)
    ("B" "Minor Bindings" which-key-show-full-minor-mode-keymap)
    ("d" "Descbinds" describe-bindings) ; or embark-bindings
    ("t" "Top Bindings  " which-key-show-top-level)
    ]
   ["Describe"
    ("C" "Command" helpful-command)
    ("f" "Function" helpful-callable)
    ("v" "Variable " helpful-variable)
    ("k" "Key" helpful-key)
    ("s" "Symbol" helpful-symbol)
    ("l" "Library" apropos-library)
    ]
   ["Info on"
    ("C-c" "Command" Info-goto-emacs-command-node)
    ("C-f" "Function" info-lookup-symbol)
    ("C-v" "Variable" info-lookup-symbol) ; fails if transient-detect-key-conflicts
    ("C-k" "Key" Info-goto-emacs-key-command-node)
    ("C-s" "Symbol" info-lookup-symbol)
    ]
   ["Goto Source"
    ""
    ("F" "Function" find-function-other-frame)
    ("V" "Variable" find-variable-other-frame)
    ("K" "Key" find-function-on-key-other-frame)
    ""
    ("L" "Library" find-library-other-frame)
    ]
   ["Apropos"
    ("ac" "Command" apropos-command)
    ("af" "Function" apropos-function)
    ("av" "Variable" apropos-variable)
    ("aV" "Value" apropos-value)
    ("aL" "Local Value" apropos-local-value)
    ("ad" "Documentation" apropos-documentation)
    ]
   ]
  [
   ["Internals"
    ("I" "Input Method" describe-input-method)
    ("G" "Language Env" describe-language-environment)
    ("S" "Syntax" describe-syntax)
    ("T" "Categories" describe-categories)
    ("O" "Coding System" describe-coding-system)
    ("o" "Coding Briefly" describe-current-coding-system-briefly)
    ("T" "Display Table" describe-current-display-table)
    ("e" "Echo Messages" view-echo-area-messages)
    ("H" "Lossage" view-lossage)
    ]
   ["Describe"
    ("." "At Point" helpful-at-point)
    ("c" "Key Short" describe-key-briefly)
    ("p" "Key Map" describe-keymap)
    ("A" "Face" describe-face)
    ("i" "Icon" describe-icon)
    ("w" "Where Is" where-is)
    ("=" "Position" what-cursor-position)
    ("g" "Shortdoc" shortdoc-display-group)
    ]
   ["Info Manuals"
    ("C-i" "Info" info)
    ("C-4" "Other Window" info-other-window)
    ("C-e" "Emacs" info-emacs-manual)
    ;; ("C-l" "Elisp" info-elisp-manual)
    ("C-r" "Pick Manual" info-display-manual)
    ]
   ["External"
    ;; ("N" "Man" consult-man)
    ;; ("W" "Dictionary" lookup-word-at-point)
    ;; ("D" "Dash" dash-at-point)
    ]
   ]
  )
(global-set-key (kbd "C-S-h") 'hrm-help-transient)

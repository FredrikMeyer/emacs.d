;;; fm-clojure.el --- Clojure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package flycheck-clj-kondo
  :ensure t
  :after (flycheck))


;; key bindings and code colorization for Clojure
;; https://github.com/clojure-emacs/clojure-mode
(use-package clojure-mode
  :ensure t
  :ensure-system-package clojure
  :mode "\\.edn$"
  :mode "\\.boot$"
  :mode "\\.clj$"
  :hook ((clojure-mode . subword-mode) ;; For Java class names
         (clojure-mode . electric-indent-mode)
         (clojure-mode . electric-pair-mode)
         (clojure-mode . smartparens-strict-mode)
         (clojure-mode . cider-mode)
         ;; (clojure-mode . paredit-mode)
         )
  :config
  (require 'flycheck-clj-kondo)
  ;; Enable paredit for Clojure
  ;; (add-hook 'clojure-mode-hook 'enable-paredit-mode)
  (add-to-list 'auto-mode-alist '("\\.cljs.*$" . clojurescript-mode))
  (add-to-list 'auto-mode-alist '("lein-env" . enh-ruby-mode)))

;; extra syntax highlighting for clojure
(use-package clojure-mode-extra-font-locking
  :after clojure-mode
  :ensure t)

;; Integration with a Clojure REPL
;; https://github.com/clojure-emacs/cider
(use-package cider
  :ensure t
  :hook ((cider-mode . eldoc-mode)
         (cider-mode . company-mode)
         (cider-repl-mode . company-mode))
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

(provide 'fm-clojure)
;;; fm-clojure.el ends here

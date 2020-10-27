;;; fm-common-lisp.el --- Common lisp -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Code:

(add-auto-mode 'lisp-mode "\\.cl\\'")

(use-package slime
  :ensure t
  :hook (lisp . slime)
  ;; :init
  ;; (load (expand-file-name "~/quicklisp/slime-helper.el"))
  :config
  (setq inferior-lisp-program "/usr/local/bin/sbcl")
  (slime-setup '(slime-fancy slime-company)))
;; (setq slime-contribs '(slime-fancy slime-repl)))

(use-package ac-slime
  :disabled
  :ensure t
  :hook ((slime-mode . ac-mode)
         (slime-mode . set-up-slime-ac)
         (slime-repl-mode . set-up-slime-ac))
  :config
  (add-to-list 'ac-modes 'slime-repl-mode))

(use-package slime-company
  :ensure t
  :after (slime company)
  :config (setq slime-company-completion 'fuzzy
                slime-company-after-completion 'slime-company-just-one-space)
  (setq company-idle-delay 0.3)
  (setq company-box-doc-delay 1))

(provide 'fm-common-lisp)
;;; fm-common-lisp ends here

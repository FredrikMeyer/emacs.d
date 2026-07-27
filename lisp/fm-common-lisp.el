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

(provide 'fm-common-lisp)
;;; fm-common-lisp.el ends here

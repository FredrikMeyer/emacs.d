;;; fm-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


;;; Code:

(use-package elpy
  :disabled
  :ensure t
  ;; :after pyenv-mode
  :init (elpy-enable)
  :config
  (add-hook 'pyenv-mode-hook (lambda () (elpy-enable)))
  (add-hook 'elpy-mode-hook 'flycheck-mode)
  (setq elpy-rpc-backend "jedi")
  (when (load "flycheck" t t)
    (setq elpy-modules (delq 'elpy-module-flymake elpy-modules))))

(use-package flycheck-pycheckers
  :defer 2
  :disabled
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))


(use-package python-pytest
  :defer 2
  :ensure t)

(setq python-shell-interpreter "python3")

(use-package pyvenv
  :defer 2
  :ensure t
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (setq python-shell-interpreter (concat pyvenv-virtual-env "bin/python3")))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(add-hook 'python-mode 'electric-pair-mode)



(provide 'fm-python)
;;; fm-python ends here

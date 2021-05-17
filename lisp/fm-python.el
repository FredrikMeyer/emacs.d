;;; fm-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:
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

(use-package lsp-pyright
  ;; :disabled ;; test python-lsp again 
  :ensure t
  :hook (python-mode . (lambda ()
                          (require 'lsp-pyright)
                          (lsp)))
  :config
  (setq lsp-pyright-typechecking-mode "basic")
  ;; (setq lsp-pyright)
  )  ; or lsp-deferred


(use-package python-pytest
  :defer 2
  :ensure t
  :config
  (setq python-pytest-executable "python -m pytest")

  (add-hook 'python-mode-hook
            (lambda ()
              (when-let ((r (locate-dominating-file default-directory ".pyroot")))
                (setq python-pytest-executable
                      (concat "PYTHONPATH=" r " " "pytest"))))))

(use-package python-docstring
  :ensure t
  :config
  (python-docstring-install))

(setq python-shell-interpreter "python3")

(use-package python-black
  :ensure t
  :after python)

(use-package py-autopep8
  :ensure t
  :config
  (setq py-autopep8-options '("--max-line-length=120")))

(use-package py-isort
  :ensure t)

(use-package pyvenv
  :defer 2
  :ensure t
  :config
  (pyvenv-mode t)

  ;; Set correct Python interpreter
  ;; TODO: auto detect virtual env (f.ex via a dotfile)
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (let
                    ((ipython (concat pyvenv-virtual-env "bin/ipython"))
                     (python (concat pyvenv-virtual-env "bin/python3")))
                  (if (file-exists-p ipython)
                      (progn
                        (setq python-shell-interpreter ipython)
                        (setq python-shell-interpreter-args "--simple-prompt -i")
                        (setq org-babel-python-command python)
                        )
                    (progn
                      (setq org-babel-python-command python)
                      (setq python-shell-interpreter python)))))))
  (setq pyvenv-post-deactivate-hooks
        (list (lambda ()
                (setq python-shell-interpreter "python3")))))

(add-hook 'python-mode 'electric-pair-mode)


(provide 'fm-python)
;;; fm-python ends here

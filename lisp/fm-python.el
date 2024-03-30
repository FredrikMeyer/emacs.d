;;; fm-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:

;; My Python specific configuration

;;; Code:

(use-package flycheck-pycheckers
  :defer 2
  :disabled
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))

(use-package lsp-pyright
  :disabled
  :ensure t
  :defer t
  :hook (python-mode . (lambda ()
                         (require 'lsp-pyright)
                         (lsp)
                         (flycheck-add-next-checker 'lsp 'python-mypy)))
  :init
  (setq lsp-pyright-typechecking-mode "basic")
  ;; (setq lsp-pyright-venv-directory "~/.pyenv/versions/3.8.7/envs/")
  )


(use-package python-pytest
  :ensure t
  :bind (:map python-mode-map
              ("C-c t" . 'python-pytest-dispatch))
  ;; :hook (python-mode . python-pytest-mode)
  :config
  (setq python-pytest-executable "python -m pytest -o log_cli=true")

  (add-hook 'python-mode-hook
            (lambda ()
              (when-let ((r (locate-dominating-file default-directory ".pyroot")))
                (setq python-pytest-executable
                      (concat "PYTHONPATH=" r " " "python -m pytest -o log_cli=true"))))))

(use-package python-black
  :ensure t
  :commands (python-black-buffer)
  ;; https://emacs.stackexchange.com/a/59270/20796
  :bind ((:map python-mode-map ("C-c C-b" . python-black-buffer)))
  :after python)

(use-package py-autopep8
  :ensure t
  :commands (py-autopep8-buffer py-autopep8-region)
  :init
  (setq py-autopep8-options '("--max-line-length=120")))

(use-package py-isort
  :ensure t
  :after python)

(use-package pyvenv
  :ensure t
  :init
  ;; Set correct Python interpreter
  ;; TODO: auto detect virtual env (f.ex via a dotfile)
  (setq pyvenv-post-activate-hooks
        (list (lambda ()
                (let
                    ((ipython (concat pyvenv-virtual-env "bin/ipython"))
                     (python (concat pyvenv-virtual-env "bin/python3")))
                  (if nil ;; (file-exists-p ipython)
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
                (setq python-shell-interpreter "python3"))))
  :config
  (pyvenv-mode t))

;; (add-hook 'python-mode (lambda () (flycheck-add-next-checker 'lsp 'python-flake8)))

(use-package python
  :after lsp-mode
  :ensure t
  :hook (python-mode . electric-pair-mode)
  :init
  (setq python-shell-interpreter "python3")
  :config
  (add-hook 'python-mode 'eglot-ensure)
  ;; (flycheck-add-next-checker 'lsp 'python-flake8)
  (setq python-indent-offset 4))

(defun activate-flake8 ()
  (interactive)
  (flycheck-add-next-checker 'lsp 'python-flake8)
  )

(provide 'fm-python)
;;; fm-python.el ends here

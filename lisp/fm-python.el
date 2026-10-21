;;; fm-python.el --- Python editing -*- lexical-binding: t -*-
;;; Commentary:

;; My Python specific configuration

;;; Code:

(require 'subr-x)

(defun fm-python--project-root ()
  "Return the current Python project root, if any."
  (or (when-let* ((project (project-current nil)))
        (project-root project))
      (locate-dominating-file default-directory ".python-version")
      (locate-dominating-file default-directory ".venv")))

(defun fm-python--project-venv-python (project-root)
  "Return PROJECT-ROOT's local virtualenv Python, if it exists."
  (let ((python (expand-file-name
                 (if (eq system-type 'windows-nt)
                     "Scripts/python.exe"
                   "bin/python")
                 (expand-file-name ".venv" project-root))))
    (when (file-executable-p python)
      python)))

(defun fm-python--pyenv-python (project-root)
  "Return the pyenv Python named by PROJECT-ROOT/.python-version, if it exists."
  (let* ((python-version-file (expand-file-name ".python-version" project-root))
         (pyenv-root (file-name-as-directory
                      (or (getenv "PYENV_ROOT")
                          (expand-file-name "~/.pyenv")))))
    (when (file-readable-p python-version-file)
      (let* ((version-name (string-trim
                            (with-temp-buffer
                              (insert-file-contents python-version-file)
                              (buffer-string))))
             (python (expand-file-name
                      (format "versions/%s/bin/python" version-name)
                      pyenv-root)))
        (when (file-executable-p python)
          python)))))

(defun fm-python-configure-project-environment ()
  "Configure Python and pyright to use the current project's interpreter."
  (when-let* ((project-root (fm-python--project-root))
              (python (or (fm-python--project-venv-python project-root)
                          (fm-python--pyenv-python project-root))))
    (setq-local python-shell-interpreter python)
    (setq-local lsp-pyright-python-executable-cmd python)
    (let ((python-bin-dir (file-name-directory python)))
      (setq-local exec-path (cons python-bin-dir
                                  (remove python-bin-dir exec-path))))))

(defun fm-python-start-lsp ()
  "Start pyright after configuring the project Python interpreter."
  (require 'lsp-pyright)
  (fm-python-configure-project-environment)
  (lsp-deferred))

(use-package flycheck-pycheckers
  :defer 2
  :disabled
  :ensure t
  :config
  (with-eval-after-load 'flycheck
    (add-hook 'flycheck-mode-hook #'flycheck-pycheckers-setup)))

(use-package lsp-pyright
  :ensure t
  :defer t
  :hook ((python-mode python-ts-mode) . fm-python-start-lsp)
  :init
  (setq lsp-pyright-multi-root nil)
  :custom
  (lsp-pyright-type-checking-mode "basic")

  )


(use-package python-pytest
  :ensure t
  :bind (:map python-mode-map
              ("C-c t" . 'python-pytest-dispatch))
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

(use-package virtualenvwrapper
  :ensure t
  :config
  (venv-initialize-interactive-shells) ;; if you want interactive shell support
  (venv-initialize-eshell) ;; if you want eshell support
  (setq venv-location "~/.pyenv/versions"))

(use-package pyvenv
  :ensure t
  :hook (python-mode . pyvenv-mode)
  :commands (pyvenv-activate pyvenv-deactivate pyvenv-workon)
  :init
  ;; Set correct Python interpreter
  ;; TODO: auto detect virtual env (f.ex via a dotfile)
  (setenv "WORKON_HOME" "~/.pyenv/versions")
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
  :hook ((python-mode . electric-pair-mode))
  :custom
  (python-shell-interpreter "python3")
  (python-indent-offset 4)
  :config
  (require 'lsp-diagnostics)
  ;; (lsp-diagnostics-flycheck-enable)
  ;; (flycheck-add-next-checker 'lsp 'python-flake8)
  )

(defun activate-flake8 ()
  (interactive)
  (flycheck-add-next-checker 'lsp 'python-flake8)
  )

(defun uv-activate ()
  "Activate Python environment managed by uv based on current project directory.
Looks for .venv directory in project root and activates the Python interpreter."
  (interactive)
  (let* ((project-root (project-root (project-current t)))
         (venv-path (expand-file-name ".venv" project-root))
         (python-path (expand-file-name
                       (if (eq system-type 'windows-nt)
                           "Scripts/python.exe"
                         "bin/python")
                       venv-path)))
    (if (file-exists-p python-path)
        (progn
          ;; Set Python interpreter path
          (setq python-shell-interpreter python-path)

          ;; Update exec-path to include the venv's bin directory
          (let ((venv-bin-dir (file-name-directory python-path)))
            (setq exec-path (cons venv-bin-dir
                                  (remove venv-bin-dir exec-path))))

          ;; Update PATH environment variable
          (setenv "PATH" (concat (file-name-directory python-path)
                                 path-separator
                                 (getenv "PATH")))

          ;; Update VIRTUAL_ENV environment variable
          (setenv "VIRTUAL_ENV" venv-path)

          ;; Remove PYTHONHOME if it exists
          (setenv "PYTHONHOME" nil)

          (message "Activated UV Python environment at %s" venv-path))
      (error "No UV Python environment found in %s" project-root))))

(provide 'fm-python)
;;; fm-python.el ends here

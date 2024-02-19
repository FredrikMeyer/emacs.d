;;; fm-rust.el --- Clojure -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; For later
;; https://robert.kra.hn/posts/rust-emacs-setup/
;; https://github.com/brotzeit/rustic

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

(provide 'fm-rust)
;;; fm-rust.el ends here

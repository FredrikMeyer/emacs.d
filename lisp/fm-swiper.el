;;; fm-swiper.el --- Ivy -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package ivy
  :ensure t
  :demand t
  ;; :pin melpa
  :hook ((emacs-startup . ivy-mode)
         (after-init . ivy-mode))
  :diminish (ivy-mode)
  :bind (("C-c C-r" . ivy-resume)
         ("M-g o" . 'counsel-outline)
         ("C-x b" . ivy-switch-buffer))
  :custom
  (ivy-use-virtual-buffers t)
  (enable-recursive-minibuffers t)
  :init
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy)
  (setq ivy-wrap t)
  ;; Fuzzy matching is the best
  (setq ivy-re-builders-alist
        '((counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          ;; (swiper-isearch . ivy--regex-plus)
          ;; (counsel-projectile-find-file . ivy--regex-plus)
          (counsel-M-x . ivy--regex-plus)
          (t . ivy--regex-plus))))

(use-package counsel
  :after ivy
  :ensure t
  :hook ((after-init . counsel-mode))
  :bind
  (("M-y" . counsel-yank-pop)
   :map ivy-minibuffer-map
   ("M-y" . ivy-next-line)
   ("M-x" . counsel-M-x)
   ("RET" . ivy-alt-done)
   ("C-x C-f" . counsel-find-file)
   ("C-h v" . counsel-describe-variable)
   ("C-h f" . counsel-describe-function)
   ("C-h o" . counsel-describe-symbol)
   ("<backspace>" . 'ivy-backward-delete-char))
  :config
  (setq counsel-rg-base-command
        '("rg"
          "--hidden"
          "--glob" "!.git"
          "--max-columns" "240"
          "--with-filename"
          "--no-heading"
          "--line-number"
          "--color" "never"
          "%s"))
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  (setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) "")
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-expression-history)

  (defun my/projectile-counsel-rg ()
    "Search the current Projectile project with `counsel-rg'."
    (interactive)
    (let ((default-directory (projectile-project-root)))
      (counsel-rg)))

  (with-eval-after-load 'projectile
    (define-key projectile-command-map (kbd "s r") #'my/projectile-counsel-rg)))


;; https://github.com/tumashu/ivy-posframe
(use-package ivy-posframe
  :ensure t
  :hook (ivy-mode . ivy-posframe-mode))

;; https://github.com/radian-software/prescient.el
;; Simple but effective sorting and filtering for Emacs.
(use-package ivy-prescient
  :ensure t
  ;; :disabled
  :after ivy
  :hook ((after-init . ivy-prescient-mode)
         (after-init . prescient-persist-mode))
  :custom
  (prescient-history-length 50))

;; https://github.com/Yevgnen/ivy-rich
(use-package ivy-rich
  :after ivy
  :ensure t
  :hook (all-the-icons-ivy-rich-mode . ivy-rich-mode)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; https://github.com/asok/all-the-icons-ivy
(use-package all-the-icons-ivy-rich
  ;; :hook (counsel-projectile-mode . all-the-icons-ivy-rich-mode)
  :ensure t)

; https://github.com/ericdanan/counsel-projectile
(use-package counsel-projectile
  :disabled
  :defer 1
  :ensure t
  :hook (ivy-mode . counsel-projectile-mode))

(use-package avy
  :ensure t
  :bind (("C-:" . 'avy-goto-char)
          ("M-g f" . 'avy-goto-line))
  )


(provide 'fm-swiper)

;;; fm-swiper.el ends here

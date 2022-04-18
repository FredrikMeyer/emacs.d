;;; fm-swiper.el --- Ivy -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:


(use-package ivy
  :ensure t
  :demand t
  ;; :pin melpa
  :hook (emacs-startup . ivy-mode)
  :diminish (ivy-mode)
  :bind (("C-c C-r" . ivy-resume)
         ("C-x b" . ivy-switch-buffer))
  :config
  (ivy-mode 1)
  (setq ivy-use-virtual-buffers t)
  (setq enable-recursive-minibuffers t)
  (setq ivy-count-format "%d/%d ")
  (setq ivy-display-style 'fancy)
  (setq ivy-wrap t)
  ;; Fuzzy matching is the best
  (setq ivy-re-builders-alist
        '((counsel-ag . ivy--regex-plus)
          (counsel-rg . ivy--regex-plus)
          (swiper-isearch . ivy--regex-plus)
          (counsel-projectile-find-file . ivy--regex-plus)
          (counsel-M-x . ivy--regex-plus)
          (t . ivy--regex-plus))))

(use-package counsel
  :after ivy
  :ensure t
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
  (counsel-mode)
  (setq counsel-describe-function-function #'helpful-callable)
  (setq counsel-describe-variable-function #'helpful-variable)
  (setf (alist-get 'counsel-M-x ivy-initial-inputs-alist) "")
  (define-key minibuffer-local-map (kbd "C-r") 'counsel-expression-history))

(use-package counsel-osx-app
  :ensure t
  :bind ("S-M-SPC" . counsel-osx-app)
  :commands counsel-osx-app)


;; https://github.com/tumashu/ivy-posframe
(use-package ivy-posframe
  :ensure t
  :hook (ivy-mode . ivy-posframe-mode))

(use-package ivy-prescient
  :ensure t
  :config
  (ivy-prescient-mode t))

(use-package swiper
  :ensure t
  :after ivy
  :bind (("C-s" . swiper-isearch)
	 ("C-r" . swiper-isearch)
	 ("C-x C-f" . counsel-find-file)))

;; https://github.com/Yevgnen/ivy-rich
(use-package ivy-rich
  :after ivy
  :ensure t
  :hook (all-the-icons-ivy-rich-mode . ivy-rich-mode)
  :config
  (setcdr (assq t ivy-format-functions-alist) #'ivy-format-function-line))

;; https://github.com/asok/all-the-icons-ivy
(use-package all-the-icons-ivy-rich
  :hook (counsel-projectile-mode . all-the-icons-ivy-rich-mode)
  :ensure t)

; https://github.com/ericdanan/counsel-projectile
(use-package counsel-projectile
  :defer 1
  :ensure t
  :hook (ivy-mode . counsel-projectile-mode))


(provide 'fm-swiper)

;;; fm-swiper.el ends here

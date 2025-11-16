;;; early-init.el --- early bird  -*- no-byte-compile: t -*-


;;; Code:

;; From https://emacs.stackexchange.com/questions/5636/when-installing-a-package-with-package-el-how-to-never-show-the-compile-log-buf
;; Tested working to avoid site-lisp byte-compile warning buffer being displayed at start
(setq byte-compile-warnings nil)

(setq load-prefer-newer t)

(when (fboundp 'scroll-bar-mode)
  (scroll-bar-mode -1))

;; increase font size for better readability
;; (set-face-attribute 'default nil
                    ;; :height 140
                    ;; :weight 'normal
                    ;; :family "Jetbrains Mono"
                    ;; )

;; Removes tool-bar
(if (fboundp 'tool-bar-mode) (tool-bar-mode -1))

;; https://github.com/d12frosted/homebrew-emacs-plus/issues/323#issuecomment-1382265470
(setenv "LIBRARY_PATH"
         (mapconcat 'identity '("/opt/homebrew/Cellar/gcc/15.1.0"
                                "/opt/homebrew/Cellar/libgccjit/15.1.0/lib/gcc/current"
                                "/opt/homebrew/Cellar/gcc/15.1.0/lib/gcc/current/gcc/aarch64-apple-darwin24/15")
                   ":"))

;; (setq initial-frame-alist '((top . 0) (left . 0) (width . 177) (height . 53)))

;;(setq package-enable-at-startup t)
;; (package-initialize)

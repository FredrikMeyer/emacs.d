;;; fm-diverse.el --- Diverse -*- lexical-binding: t -*-
;;; Commentary:
;;; Code:

;; https://github.com/rejeep/prodigy.el
(use-package prodigy
  :ensure t
  :config
  (prodigy-define-service
    :name "Jekyll serve"
    :command "bundle"
    :args '("exec" "jekyll" "serve" "-l")
    :cwd "~/code/FredrikMeyer.github.io/"
    :port 4000
    )
  )

;; https://github.com/TeMPOraL/nyan-mode
(use-package nyan-mode
  :ensure t
  :custom
  (nyan-wavy-trail 1)
  :config
  (nyan-mode))

(use-package jira
  :ensure t
  :commands (jira-issues)
  :config
  (setq jira-base-url "https://nav.atlassian.net/")
  (setq jira-username "fredrik.meyer@nav.no") ;; Jira username (usually, an email)
  ;; API token for Jira
  ;; See https://support.atlassian.com/atlassian-account/docs/manage-api-tokens-for-your-atlassian-account/
  (setq jira-token (getenv "JIRA_API_TOKEN"))
  (setq jira-token-is-personal-access-token nil)
  (setq jira-api-version 3) ;; Version 2 is also allowed
  ;; (Optional) API token for JIRA TEMPO plugin
  ;; See https://apidocs.tempo.io/
  ;; (setq jira-tempo-token "foobar123123")
  )

;; https://depp.brause.cc/nov.el/
(use-package nov
  :mode ("\\.epub$" . nov-mode)
  :ensure t)

;; https://github.com/johanvts/emacs-fireplace
(use-package fireplace
  :commands (fireplace)
  :ensure t)

(provide 'fm-diverse)
;;; fm-diverse.el ends here

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
  :custom
  (jira-base-url "https://nav.atlassian.net/")
  (jira-username "fredrik.meyer@nav.no") ;; Jira username (usually, an email)
  (jira-token (getenv "JIRA_API_TOKEN"))
  (jira-token-is-personal-access-token nil)
  (jira-api-version 3) ;; Version 2 is also allowed
  :commands (jira-issues)
  :config

  ;; API token for Jira
  ;; See https://support.atlassian.com/atlassian-account/docs/manage-api-tokens-for-your-atlassian-account/
  
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

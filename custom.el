(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(package-selected-packages
   '(ace-window add-node-modules-path agent-shell agent-shell-dashboard all-the-icons-completion all-the-icons-dired
                all-the-icons-ivy-rich ansible anzu applescript-mode auctex auto-complete blamer breadcrumb buffer-box
                buffer-move cargo casual casual-lib cfrs claude-code-ide clj-refactor clojure-mode-extra-font-locking
                clojure-snippets consult copilot corfu dashboard dired-preview dired-subtree djvu docker dockerfile-mode
                doom-modeline dumb-jump easy-kill edraw-org el-easydraw embark embark-consult ess exec-path-from-shell
                expand-region fireplace flycheck-clj-kondo flycheck-color-mode-line flycheck-plantuml flycheck-posframe
                forge frame-local geiser-racket git-commit git-gutter git-link git-messenger git-timemachine glsl-mode
                gnuplot go-mode gptel-magit graphviz-dot-mode helpful hl-todo htmlize hydra image+ inflections
                ivy-posframe ivy-prescient jinja2-mode jinx jira js2-mode json-mode justl k8s-mode keyfreq kind-icon
                know-your-http-well kotlin-mode kubernetes kv lean4-mode lsp-java lsp-metals lsp-pyright lsp-ui magit-gh
                marginalia mathjax mcp mermaid-mode minions modus-themes multiple-cursors nand2tetris nand2tetris-mode
                nov nyan-mode ob-http orderless org-ai org-block-capf org-contrib org-download org-draw org-modern
                org-noter org-rich-yank org-roam-ui org-super-agenda org-superstar orgtbl-aggregate package-utils
                pandoc-mode pdf-tools peep-dired pfuture phi-search pinentry pkg-info popwin prettier-js prodigy
                projectile py-autopep8 py-isort python-black python-pytest pyvenv racket-mode rainbow-delimiters
                ready-player restclient rg rust-mode sbt-mode scala-ts-mode slime smart-hungry-delete smartparens
                spacemacs-theme system-packages toc-org try undo-tree vertico virtualenvwrapper vterm-toggle web-mode
                whisper-api yasnippet-snippets))
 '(package-vc-selected-packages
   '((org-block-capf :url "https://github.com/xenodium/org-block-capf")
     (agent-shell-dashboard :url "https://github.com/wandersoncferreira/agent-shell-dashboard" :branch "main")))
 '(safe-local-variable-values
   '((org-latex-pdf-process "%o/svg2pdf.sh %o" "latexmk -pdf -interaction=nonstopmode -output-directory=%o %f")
     (nand2tetris-tools-dir . "/Users/fredrikmeyer/code/nand2tetris/nand2tetris/tools")
     (python-shell-interpreter . ".venv/bin/python") (python-shell-interpreter-args . "-i")
     (python-pytest-executable . "uv run pytest") (python-shell-interpreter . "uv")
     (python-shell-interpreter-args . "run python -i")))
 '(warning-suppress-log-types '((files missing-lexbind-cookie))))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(agent-shell-dashboard-attention ((t :foreground "#972500" :weight bold)))
 '(agent-shell-dashboard-badge-done ((t :foreground "#00663f" :background "#b3fabf" :box (:line-width (1 . -1) :color "#00663f") :weight bold)))
 '(agent-shell-dashboard-badge-ready ((t :foreground "#2a5045" :box (:line-width (1 . -1) :color "#9f9f9f"))))
 '(agent-shell-dashboard-badge-waiting ((t :foreground "#972500" :background "#ffcfbf" :box (:line-width (1 . -1) :color "#972500") :weight bold)))
 '(agent-shell-dashboard-badge-working ((t :foreground "#884900" :background "#fff576" :box (:line-width (1 . -1) :color "#884900") :weight bold)))
 '(agent-shell-dashboard-badge-wt ((t :foreground "#005f5f" :background "#bfefff" :box (:line-width (1 . -1) :color "#005f5f") :weight bold)))
 '(agent-shell-dashboard-banner ((t :foreground "#3548cf" :weight bold)))
 '(agent-shell-dashboard-dim ((t :foreground "#595959")))
 '(agent-shell-dashboard-heading-actions ((t :foreground "#005f5f" :weight bold)))
 '(agent-shell-dashboard-heading-projects ((t :foreground "#884900" :weight bold)))
 '(agent-shell-dashboard-heading-sessions ((t :foreground "#3548cf" :weight bold)))
 '(agent-shell-dashboard-key ((t :foreground "#3548cf" :weight bold)))
 '(agent-shell-dashboard-model ((t :foreground "#531ab6")))
 '(agent-shell-dashboard-quote ((t :foreground "#595959" :slant italic)))
 '(agent-shell-dashboard-subtitle ((t :foreground "#595959"))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default default default italic underline success warning error])
 '(calendar-week-start-day 1)
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-safe-themes
   '("2422e84e81ce5ff243b9b8dd4076b8bab9b5c630c9b8a7533ec3c5b3fed23329" "96c56bd2aab87fd92f2795df76c3582d762a88da5c0e54d30c71562b7bf9c605" "7ea491e912d419e6d4be9a339876293fff5c8d13f6e84e9f75388063b5f794d6" "890a1a44aff08a726439b03c69ff210fe929f0eff846ccb85f78ee0e27c7b2ea" "378d52c38b53af751b50c0eba301718a479d7feea5f5ba912d66d7fe9ed64c8f" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "52588047a0fe3727e3cd8a90e76d7f078c9bd62c0b246324e557dfa5112e0d0c" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "1b6f7535c9526a5dbf9fb7e3604d0280feb7a07b970caf21ebd276ddc93ef07a" "c433c87bd4b64b8ba9890e8ed64597ea0f8eb0396f4c9a9e01bd20a04d15d358" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "cf08ae4c26cacce2eebff39d129ea0a21c9d7bf70ea9b945588c1c66392578d1" "5ee12d8250b0952deefc88814cf0672327d7ee70b16344372db9460e9a0e3ffc" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "9e54a6ac0051987b4296e9276eecc5dfb67fdcd620191ee553f40a9b6d943e78" "aaffceb9b0f539b6ad6becb8e96a04f2140c8faa1de8039a343a4f1e009174fb" "8aebf25556399b58091e533e455dd50a6a9cba958cc4ebb0aab175863c25b9a4" "d677ef584c6dfc0697901a44b885cc18e206f05114c8a3b7fde674fce6180879" default))
 '(debug-on-error nil)
 '(electric-indent-mode t)
 '(electric-pair-mode t)
 '(elpy-rpc-python-command "python3")
 '(fci-rule-color "#eee8d5")
 '(flycheck-javascript-flow-args nil)
 '(global-company-mode t)
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   (--map
    (solarized-color-blend it "#fdf6e3" 0.25)
    '("#b58900" "#2aa198" "#dc322f" "#6c71c4" "#859900" "#cb4b16" "#268bd2")))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
     ("#b3c34d" . 20)
     ("#6ccec0" . 30)
     ("#74adf5" . 50)
     ("#e1af4b" . 60)
     ("#fb7640" . 70)
     ("#ff699e" . 85)
     ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#e1af4b" "#fb7640" "#ff6849" "#ff699e" "#8d85e7" "#74adf5" "#6ccec0" "#b3c34d"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(hl-sexp-background-color "#efebe9")
 '(lsp-ui-doc-position 'at-point)
 '(minions-mode t)
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(org-structure-template-alist
   '(("a" . "export ascii")
     ("c" . "center")
     ("C" . "comment")
     ("e" . "example")
     ("E" . "export")
     ("h" . "export html")
     ("l" . "export latex")
     ("q" . "quote")
     ("s" . "src")
     ("v" . "verse")))
 '(package-selected-packages
   '(ligature org-superstar org-super-agenda buffer-move ob-http lsp-pyright company-restclient python-docstring package-utils-upgrade-all-and-recompile package-utils ob-ipython cfn-lint ivy-posframe magit-todos treemacs-magit treemacs-icons-dired treemacs-projectile org-projectile texfrag textfrag xwwp-follow-link xwwp-follow-link-with-ivy xwwp rainbow-blocks omnisharp clj-refactor modus-vivendi-theme modus-operandi-theme org-roam cargo fennel-mode slime-company ac-slime slime flycheck-color-mode-line eldoc-box popwin helpful swiper plantuml-mode csharp-mode vue-mode typescript wakatime-mode dap-mode forge quelpa-use-package quelpa julia-mode org-plus-contrib use-package-ensure-system-package use-package dap-java doom-modeline lsp-ui lsp-mode python-pytest flycheck-pycheckers esup benchmark-init pdf-tools org-pdftools pdftools leuven-theme dockerfile-mode elpy paredit-everywhere paredit-everywhere-mode web-mode company-box smartparens dimmer indium company-tern tern json-navigator yaml-mode dashboard anzu spacemacs-light spacemacs-theme ubuntu-theme yasnippet-snippets yasnippet-classic-snippets undo-tree kotlin-mode gradle-mode let-alist dired-subtree graphql-mode ivy-rich rust-mode chess git-messenger all-the-icons-ivy intero all-the-icons-dired all-the-icons counsel-projectile gnuplot company-tabnine company-flow flow-minor-mode projectile-ripgrep rg git-gutter+ git-gutter-+ add-node-modules-path :nyan-mode company-auctex ox-latex ox-beamer auc-tex eyebrowse org-tempo elfeed xref-js2 fireplace ace-window edit-indirect nyan-mode smart-hungry-delete hungry-delete expand-region minimap glsl-mode org-reveal minions dracula-theme solarized-theme neotree go-mode haskell-mode ruby-electric inf-ruby try which-key htmlize restclient json-mode sml-mode markdown-mode tagedit rainbow-delimiters projectile paredit magit exec-path-from-shell clojure-mode-extra-font-locking cider))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(python-shell-exec-path '("python3"))
 '(safe-local-variable-values
   '(
     (eval message "Project directory set to `%s'." my-project-path)
     (eval set
           (make-local-variable 'my-project-path)
           (file-name-directory
            (let
                ((d
                  (dir-locals-find-file ".")))
              (if
                  (stringp d)
                  d
                (car d)))))
     (python-shell-interpreter . "/Users/fredrikmeyer/code/numpoly/.venv/bin/python")
     (web-mode-markup-indentation . 2)
     (js-indent-level . 2)))
 '(save-place-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(texfrag-setup-alist
   '((texfrag-html html-mode)
     (texfrag-eww eww-mode)
     (texfrag-sx sx-question-mode)
     (texfrag-prog prog-mode)
     (texfrag-trac-wiki trac-wiki-mode markdown-mode)
     (texfrag-markdown markdown-mode)
     (texfrag-org org-mode)))
 '(texfrag-subdir "~/.emacs.d/texfrag-x")
 '(typescript-indent-level 2)
 '(use-package-compute-statistics t)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
     (40 . "#cb4366eb20b4")
     (60 . "#c1167942154f")
     (80 . "#b58900")
     (100 . "#a6ae8f7c0000")
     (120 . "#9ed892380000")
     (140 . "#96be94cf0000")
     (160 . "#8e5397440000")
     (180 . "#859900")
     (200 . "#77679bfc4635")
     (220 . "#6d449d465bfd")
     (240 . "#5fc09ea47092")
     (260 . "#4c68a01784aa")
     (280 . "#2aa198")
     (300 . "#303498e7affc")
     (320 . "#2fa1947cbb9b")
     (340 . "#2c879008c736")
     (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(vue-modes
   '((:type template :name nil :mode vue-html-mode)
     (:type template :name html :mode vue-html-mode)
     (:type template :name jade :mode jade-mode)
     (:type template :name pug :mode pug-mode)
     (:type template :name slm :mode slim-mode)
     (:type template :name slim :mode slim-mode)
     (:type script :name nil :mode js-mode)
     (:type script :name js :mode js-mode)
     (:type script :name es6 :mode js-mode)
     (:type script :name babel :mode js-mode)
     (:type script :name coffee :mode coffee-mode)
     (:type script :name ts :mode typescript-mode)
     (:type script :name typescript :mode typescript-mode)
     (:type script :name tsx :mode typescript-tsx-mode)
     (:type style :name nil :mode css-mode)
     (:type style :name css :mode css-mode)
     (:type style :name stylus :mode stylus-mode)
     (:type style :name less :mode less-css-mode)
     (:type style :name postcss :mode css-mode)
     (:type style :name scss :mode css-mode)
     (:type style :name sass :mode ssass-mode)
     (:type i18n :name nil :mode json-mode)
     (:type i18n :name json :mode json-mode)
     (:type i18n :name yaml :mode yaml-mode)))
 '(wakatime-cli-path "/usr/local/bin/wakatime")
 '(wakatime-python-bin nil)
 '(web-mode-code-indent-offset 2)
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#657b83" "#839496"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"])
 '(yaml-indent-offset 4))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(aw-leading-char-face ((t (:foreground "red" :height 2.9))))
 '(whitespace-empty ((t (:background "selectedTextBackgroundColor" :foreground "firebrick")))))

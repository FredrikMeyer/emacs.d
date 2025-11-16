;;  -*- lexical-binding: t; -*-
;; https://github.com/flycheck/flycheck/issues/1974#issuecomment-1343495202
(require 'flycheck)


(flycheck-define-checker python-ruff
  "A Python syntax and style checker using the ruff.
To override the path to the ruff executable, set
`flycheck-python-ruff-executable'.

See URL `https://beta.ruff.rs/docs/'."
  :command ("ruff"
            "check"
            (config-file "--config" flycheck-python-ruff-config)
            "--output-format=full"
            "--stdin-filename" source-inplace
            "-")
  :standard-input t
  :error-filter (lambda (errors)
                  (let ((errors (flycheck-sanitize-errors errors)))
                    (seq-map #'flycheck-flake8-fix-error-level errors)))
  :error-patterns
  ((warning line-start
            (file-name) ":" line ":" (optional column ":") " "
            (id (one-or-more (any alpha)) (one-or-more digit)) " "
            (message (one-or-more not-newline))
            line-end))
  :modes (python-mode python-ts-mode)
  :next-checkers ((warning . python-mypy)))

(add-to-list 'flycheck-checkers 'python-ruff)

(provide 'flycheck-ruff)

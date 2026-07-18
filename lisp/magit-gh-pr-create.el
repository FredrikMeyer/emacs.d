;;; magit-gh-pr-create.el --- Create GitHub PRs from Magit via gh -*- lexical-binding: t; -*-

;;; Commentary:

;; Small extension for magit-gh.  The upstream package opens the PR
;; creation page in a browser; this adds a CLI-only flow backed by `gh pr
;; create'.

;;; Code:

(require 'magit)
(require 'magit-gh)
(require 'transient)

(defgroup magit-gh-pr-create nil
  "Create GitHub pull requests from Magit using the gh CLI."
  :group 'magit-gh
  :prefix "magit-gh-pr-create-")

(defcustom magit-gh-pr-create-default-remote "origin"
  "Default remote used when pushing an unpublished branch before PR creation."
  :type 'string
  :group 'magit-gh-pr-create)

(defcustom magit-gh-pr-create-open-after-create t
  "Whether to open the created pull request in a browser."
  :type 'boolean
  :group 'magit-gh-pr-create)

(defcustom magit-gh-pr-create-push-before-create nil
  "Whether to push an unpublished branch before PR creation.
When non-nil, push the current branch to
`magit-gh-pr-create-default-remote' without asking if the branch has no
upstream yet.  This is only the initial default; saved transient choices
take precedence."
  :type 'boolean
  :group 'magit-gh-pr-create)

(defcustom magit-gh-pr-create-remember-choices t
  "Whether to remember the transient choices used to create a PR."
  :type 'boolean
  :group 'magit-gh-pr-create)

(defconst magit-gh-pr-create-buffer-name "*magit-gh-pr-create*")

(defconst magit-gh-pr-create--remembered-args
  '("--draft" "--fill" "--push" "--no-open")
  "Transient arguments that should be remembered between PR creations.")

(defun magit-gh-pr-create--default-args ()
  "Return the default transient arguments for PR creation."
  (append
   (when magit-gh-pr-create-push-before-create
     (list "--push"))
   (unless magit-gh-pr-create-open-after-create
     (list "--no-open"))))

(defun magit-gh-pr-create--check-gh ()
  "Signal a user error unless the gh CLI is available."
  (unless (executable-find "gh")
    (user-error "GitHub CLI (gh) not found in PATH")))

(defun magit-gh-pr-create--repo-dir ()
  "Return the current repository directory."
  (or (magit-toplevel)
      (user-error "Not inside a Git repository")))

(defun magit-gh-pr-create--current-branch ()
  "Return the current branch, or signal a user error."
  (or (magit-get-current-branch)
      (user-error "Not on any branch (detached HEAD)")))

(defun magit-gh-pr-create--branch-upstream (branch)
  "Return the upstream for BRANCH, or nil."
  (magit-git-string "rev-parse" "--abbrev-ref"
                    (format "%s@{upstream}" branch)))

(defun magit-gh-pr-create--remote-names ()
  "Return configured Git remote names."
  (or (magit-list-remotes)
      (user-error "No Git remotes configured")))

(defun magit-gh-pr-create--read-base (head)
  "Read the PR base branch for HEAD."
  (let ((default-base (magit-gh--default-branch)))
    (completing-read
     (format "Create PR: %s -> " head)
     (magit-list-branch-names)
     nil nil default-base)))

(defun magit-gh-pr-create--read-title (branch)
  "Read a pull request title, defaulting from BRANCH."
  (let* ((pretty-branch (replace-regexp-in-string "[-_/]+" " " branch))
         (default-title (string-trim (capitalize pretty-branch))))
    (read-string "Title: " default-title)))

(defun magit-gh-pr-create--read-body ()
  "Read a pull request body."
  (read-string "Body: "))

(defun magit-gh-pr-create--call (program &rest args)
  "Call PROGRAM with ARGS in the current repository.
Output goes to `magit-gh-pr-create-buffer-name'."
  (let ((buffer (get-buffer-create magit-gh-pr-create-buffer-name)))
    (with-current-buffer buffer
      (let ((inhibit-read-only t))
        (erase-buffer)))
    (let ((exit-code (apply #'process-file program nil buffer t args)))
      (unless (= exit-code 0)
        (display-buffer buffer)
        (user-error "%s failed; see %s" program magit-gh-pr-create-buffer-name))
      (with-current-buffer buffer
        (string-trim (buffer-string))))))

(defun magit-gh-pr-create--push-if-needed (branch push-before)
  "Push BRANCH if needed when PUSH-BEFORE is non-nil."
  (unless (or (not push-before)
              (magit-gh-pr-create--branch-upstream branch))
    (let* ((remotes (magit-gh-pr-create--remote-names))
           (default-remote (if (member magit-gh-pr-create-default-remote
                                       remotes)
                               magit-gh-pr-create-default-remote
                             (car remotes)))
           (remote default-remote))
      (message "Pushing %s to %s before creating PR..." branch remote)
      (magit-gh-pr-create--call
       "git" "push" "-u" remote (format "HEAD:%s" branch)))))

(defun magit-gh-pr-create--url-from-output (output)
  "Extract a GitHub URL from gh PR creation OUTPUT."
  (when (string-match "https://[^[:space:]]+" output)
    (match-string 0 output)))

(defun magit-gh-pr-create--remember-choices (args)
  "Persist remembered transient choices from ARGS when requested."
  (when magit-gh-pr-create-remember-choices
    (setf (alist-get 'magit-gh-pr-create transient-values)
          (delq nil
                (mapcar (lambda (arg)
                          (and (member arg magit-gh-pr-create--remembered-args)
                               arg))
                        args)))
    (transient-save-values)))

;;;###autoload
(defun magit-gh-pr-create-create (args)
  "Create a pull request from the current branch.
ARGS are transient arguments from `magit-gh-pr-create'."
  (interactive (list (transient-args 'magit-gh-pr-create)))
  (magit-gh-pr-create--check-gh)
  (let* ((default-directory (magit-gh-pr-create--repo-dir))
         (head (magit-gh-pr-create--current-branch))
         (base (magit-gh-pr-create--read-base head))
         (draft (member "--draft" args))
         (fill (member "--fill" args))
         (push-before (if (member "--push" args)
                          t
                        (if (member "--no-push" args)
                            nil
                          magit-gh-pr-create-push-before-create)))
         (open (if (member "--no-open" args)
                   nil
                 magit-gh-pr-create-open-after-create)))
    (when (equal head base)
      (user-error "Head and base branch cannot be the same"))
    (magit-gh-pr-create--push-if-needed head push-before)
    (let* ((title (unless fill (magit-gh-pr-create--read-title head)))
           (body (unless fill (magit-gh-pr-create--read-body)))
           (gh-args (append
                     (list "pr" "create" "--base" base "--head" head)
                     (when draft (list "--draft"))
                     (if fill
                         (list "--fill")
                       (list "--title" title "--body" body))))
           (output (apply #'magit-gh-pr-create--call "gh" gh-args))
           (url (magit-gh-pr-create--url-from-output output)))
      (magit-gh-pr-create--remember-choices args)
      (when (and open url)
        (browse-url url))
      (message "Created PR%s"
               (if url (format ": %s" url) "")))))

;;;###autoload (autoload 'magit-gh-pr-create "magit-gh-pr-create" nil t)
(transient-define-prefix magit-gh-pr-create ()
  "Create a GitHub pull request for the current branch using gh."
  :value #'magit-gh-pr-create--default-args
  ["Create Pull Request"
   ("-d" "Draft" "--draft")
   ("-f" "Fill title/body from commits" "--fill")
   ("-p" "Push first" "--push")
   ("-n" "Do not open after create" "--no-open")]
  ["Action"
   ("c" "Create PR" magit-gh-pr-create-create)])

(with-eval-after-load 'magit-gh
  (transient-append-suffix 'magit-gh "w"
    '("C" "Create PR (cli)" magit-gh-pr-create)))

(provide 'magit-gh-pr-create)

;;; magit-gh-pr-create.el ends here

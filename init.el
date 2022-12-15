(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(require 'use-package)

; (add-to-list 'load-path "~/development/elisp/lsp-bridge")

(require 'yasnippet)
(yas-global-mode 1)

(require 'lsp-bridge)
(global-so-long-mode 1)
;; (global-lsp-bridge-mode)

;;; backup/autosave
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'control)
(setq-default cursor-type 'bar)
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq recentf-max-saved-items 100)
(setq nrepl-use-ssh-fallback-for-remote-hosts t)
(setq custom-safe-themes t)
(setq split-height-threshold nil) ;; only open horizontal splits (theoretically)
(setq nrepl-use-ssh-fallback-for-remote-hosts t)
(setq org-babel-clojure-backend 'cider)
(setq set-mark-command-repeat-pop t) ;; cycle thru marks w/ c-space
(setq mouse-wheel-progressive-speed nil) ;; turn off mouse acceleration
(setq read-file-name-completion-ignore-case t) ;;; case insenstive autocompletion
(setq lsp-headerline-breadcrumb-enable nil)
(setq org-startup-folded t)
(setq eldoc-echo-area-use-multiline-p nil)
(setq-default show-trailing-whitespace t)
(setq dired-kill-when-opening-new-dired-buffer t) ;; stop dired from cluttering buffer list
(setq c-default-style "k&r")
(setq create-lockfiles nil)
(setq tao-theme-use-sepia nil)
(setq path-to-ctags "/usr/bin/ctags")

;; indent with spaces
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)
(setq copilot-node-executable "~/.nvm/versions/node/v16.18.1/bin/node")
(setq dired-deletion-confirmer '(lambda (x) t))

;; indent with tabs
;; (setq-default indent-tabs-mode nil)
;; (defvaralias 'c-basic-offset 'tab-width)

;;  "Recursively add all subdirectories of lisp dir to `load-path'.
(unless (eq system-type 'windows-nt)
  (let ((default-directory  "~/.emacs.d/lisp/"))
    (normal-top-level-add-subdirs-to-load-path)))
(add-to-list 'load-path "/usr/local/share/emacs/site-lisp/mu4e")
(load-file "~/.emacs.d/lisp/copilot.el/copilot.el")
(require 'copilot)

(if (display-graphic-p)
    (scroll-bar-mode -1))

(define-key global-map (kbd "C-c r p") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-c m") 'vr/mc-mark)

(require 'yasnippet)
(yas-global-mode 1)

(add-hook 'html-mode-hook
          (lambda ()
            (setq c-basic-offset 2)))

(add-hook 'eglot-managed-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c r n") 'eglot-rename)))

(add-hook 'c-mode-hook
          (lambda ()
            (setq-local fill-column 80)
            (c-set-offset 'arglist-intro '+)
            (c-set-offset 'arglist-close 0)
            (display-fill-column-indicator-mode)))

(add-hook 'before-save-hook 'gofmt-before-save)
;; (add-hook 'python-mode-hook 'copilot-mode)
(add-hook 'python-mode-hook
          (lambda ()
            (define-key python-mode-map (kbd "C-c s") 'advent-submit)))
;; (add-hook 'python-mode-hook 'eglot-ensure)
(add-hook 'copilot-mode-hook
          (lambda ()
            (with-eval-after-load 'company
              ;; disable inline previews
              (delq 'company-preview-if-just-one-frontend company-frontends))
            (define-key copilot-completion-map (kbd "<tab>") 'copilot-accept-completion)
            (define-key copilot-completion-map (kbd "TAB") 'copilot-accept-completion)))

(add-hook 'write-file-hooks 'delete-trailing-whitespace)

;; turning off until I remember what it does
;; (setq c-offsets-alist '((arglist-cont-nonempty . +)))

; (load-file "/home/solaire/.emacs.d/lisp/aoc.el")

(defun first-time-load ()
  (package-initialize)
  (package-refresh-contents)
  (package-install 'use-package))

(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(require 'recentf)
(recentf-mode 1)
(setq recentf-max-menu-items 50)
(global-set-key "\C-x\ \C-r" 'recentf-open-files)

(defun insert-org-codeblock (language)
  (interactive "sEnter language for code block: ")
  (insert "#+BEGIN_SRC ")
  (insert language)
  (insert "\n\n#+END_SRC")
  (previous-line))

(defun dc/dired-mode-keys ()
  "User defined keys for dired mode."
  (interactive)
  (local-set-key (kbd "C-<return>") 'dired-display-file)
  (local-set-key (kbd "C-o") 'other-window))

(add-hook 'dired-mode-hook 'dc/dired-mode-keys)

(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name))))

(defun user-clojure-keybindings ()
  (if (not (bound-and-true-p cider-mode))
      (local-set-key (kbd "C-c C-c") 'inf-clojure)))

(add-hook 'clojure-mode-hook #'user-clojure-keybindings)

(defun my/select-current-line-and-forward-line (arg)
  "Select the current line and move the cursor by ARG lines IF
no region is selected.

If a region is already selected when calling this command, only move
the cursor by ARG lines."
  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (forward-line arg))

(defun launch-kitty-in-cwd ()
  (interactive)
  (let ((project-root (cdr (project-current))))
    (call-process "kitty" nil 0 nil "-d" default-directory)))

(defun launch-kitty-in-vc-root ()
  (interactive)
  (let ((project-root (cdr (project-current))))
    (call-process "kitty" nil 0 nil "-d" project-root)))

(defun launch-alacritty-in-cwd ()
  (interactive)
  (let ((project-root (cdr (project-current))))
    (shell-command "spawn-alacritty-cwd" nil nil)))

(defun launch-alacritty-in-vc-root ()
  (interactive)
  (let ((project-root (cdr (project-current))))
    (async-shell-command (concat "fish -c \"alacritty --working-directory " project-root "\""))))

(defun launch-iterm-in-vc-root ()
  (interactive)
  (let ((project-root (cdr (project-current))))
    (shell-command (concat "open -a iTerm " project-root) nil nil)))

(global-set-key (kbd "M-1") 'my/select-current-line-and-forward-line)
(global-set-key (kbd "M-2") 'mark-defun)
(global-set-key (kbd "C-w") 'backward-kill-word)
(global-set-key (kbd "C-x C-k") 'kill-region)
(global-set-key (kbd "C-x t") 'beginning-of-buffer)
(global-set-key (kbd "C-x e") 'end-of-buffer)

(add-hook 'racket-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-c") 'racket-run)))

(add-hook 'flycheck-mode-hook
          (lambda ()
            (local-set-key (kbd "S-<f2>") 'flycheck-previous-error)
            (local-set-key (kbd "<f2>") 'flycheck-next-error)))

(defun user-progmode-keybindings ()
  (local-set-key (kbd "C-c C-l") 'org-store-link))

(add-hook 'prog-mode-hook		#'user-progmode-keybindings)
;; problem exists below

(add-hook 'flymake-mode-hook
          (lambda ()
            (local-set-key (kbd "S-<f2>") 'flymake-goto-prev-error)
            (local-set-key (kbd "<f2>") 'flymake-goto-next-error)))

(defun copy-parent-dir-as-kill ()
  (interactive)
  (kill-new (expand-file-name default-directory)))

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-l"))

(global-set-key (kbd "M-o") #'er-switch-to-previous-buffer)
(global-set-key (kbd "C-M-o") #'delete-other-windows)
(global-set-key (kbd "C-o") #'other-window)
(global-set-key (kbd "C-;") #'comment-line)
(global-set-key [f2] nil)
(global-set-key (kbd "<next>") 'View-scroll-half-page-forward)
(global-set-key (kbd "<prior>") 'View-scroll-half-page-backward)
(global-set-key (kbd "C-.") 'project-find-regexp)
(global-set-key (kbd "C-\\") 'project-switch-to-buffer)
(global-set-key (kbd "C-x b") 'switch-to-buffer)
(global-set-key (kbd "M-p") (lambda () (interactive) (exchange-point-and-mark) (keyboard-quit)))
(global-set-key (kbd "C-`") #'er-switch-to-previous-buffer)
(global-set-key (kbd "M-`") #'other-frame)
(global-set-key (kbd "<C-left>") #'back-to-indentation)
(global-set-key (kbd "<C-right>") #'move-end-of-line)
(global-set-key (kbd "<mouse-4>") 'scroll-down-line)
(global-set-key (kbd "<mouse-5>") 'scroll-up-line)
(global-set-key (kbd "C-,") 'copilot-mode)
(global-unset-key (kbd "C-<next>"))

;; Interactively do things.

(setq ido-use-virtual-buffers t)
(ido-mode 1)
(ido-everywhere)
(setq ido-enable-flex-matching t)
(fido-mode)
(defadvice dired-create-directory (around inhibit-ido activate)
  "Turn off Ido mode for the duration, then turn it on."
  (unwind-protect
       (progn (ido-everywhere -1) ad-do-it)
    (ido-everywhere 1)))

(require 'mu4e)

;; use mu4e for e-mail in emacs
(setq mail-user-agent 'mu4e-user-agent)

(setq mu4e-drafts-folder "/[Gmail].Drafts")
(setq mu4e-sent-folder   "/[Gmail].Sent Mail")
(setq mu4e-trash-folder  "/[Gmail].Trash")

;; don't save message to Sent Messages, Gmail/IMAP takes care of this
(setq mu4e-sent-messages-behavior 'delete)

;; (See the documentation for `mu4e-sent-messages-behavior' if you have
;; additional non-Gmail addresses and want assign them different
;; behavior.)

;; setup some handy shortcuts
;; you can quickly switch to your Inbox -- press ``ji''
;; then, when you want archive some messages, move them to
;; the 'All Mail' folder by pressing ``ma''.

(setq mu4e-maildir-shortcuts
    '( (:maildir "/INBOX"              :key ?i)
       (:maildir "/[Gmail].Sent Mail"  :key ?s)
       (:maildir "/[Gmail].Trash"      :key ?t)
       (:maildir "/[Gmail].All Mail"   :key ?a)))

;; allow for updating mail using 'U' in the main view:
;; (setq mu4e-get-mail-command "")
(setq mu4e-get-mail-command "mbsync -a")

;; something about ourselves
(setq
 user-mail-address "eskinjp@gmail.com"
 user-full-name  "Jon Eskin"
 mu4e-compose-signature-auto-include nil)



;; sending mail -- replace USERNAME with your gmail username
;; also, make sure the gnutls command line utils are installed
;; package 'gnutls-bin' in Debian/Ubuntu

(require 'smtpmail)


;; alternatively, for emacs-24 you can use:
(setq message-send-mail-function 'smtpmail-send-it
    smtpmail-stream-type 'starttls
    smtpmail-default-smtp-server "smtp.gmail.com"
    smtpmail-smtp-server "smtp.gmail.com"
    smtpmail-smtp-service 587)

;; don't keep message buffers around
(setq message-kill-buffer-on-exit t)

;; i think this ignores annoying messages about people coming and going on IRC channels
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(blink-cursor-mode 0)
(xterm-mouse-mode 1)

;; Escape C-x and C-c in terminal mode
(add-hook 'term-mode-hook
          (lambda ()
            ;; Hack to set two escape chars.
            (let (term-escape-char)
              (term-set-escape-char ?\C-x))
            (let (term-escape-char)
              (term-set-escape-char ?\C-c))))

(define-key emacs-lisp-mode-map (kbd "<f5>") 'eval-buffer)
(add-hook 'flymake-mode-hook
          (lambda ()
            (local-set-key (kbd "M-n") 'flymake-goto-next-error)
            (local-set-key (kbd "M-p") 'flymake-goto-prev-error)))


(add-hook 'c++-mode-hook (lambda () (c-toggle-comment-style 1)))

(add-hook 'emacs-lisp-mode-hook
          '(lambda ()
            ;; (push 'company-elisp company-backends)
            (local-set-key (kbd "C-c C-c") 'eval-buffer)))

(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-r") #'compile)))


;;; allows evaluating code blocks for these languages in org mode
(org-babel-do-load-languages
 'org-babel-load-languages
 '((js . t)
   (lisp . t)
   (clojure . t)))

;;; toolbar visibility
(tool-bar-mode -1)
(menu-bar-mode -1)

; (add-to-list 'custom-theme-load-path "/home/solaire/development/elisp/emacs-valheim-theme")

(use-package diff-hl
    :ensure t)

(use-package elfeed
    :ensure t
    :config
    (setq elfeed-feeds
          '(("http://nullprogram.com/feed/" blog emacs)
            ("https://eli.thegreenplace.net/feeds/all.atom.xml" blog dev)
            ("https://www.joelonsoftware.com/feed/" blog dev)
            ("https://danluu.com/atom.xml" dev blog)
            ("https://drewdevault.com/feed.xml" blog dev)
            ("https://nrk.neocities.org/rss.xml" blog dev)
            )))

(use-package ag
    :ensure t)

(use-package avy
    :ensure t)

(use-package visual-regexp
    :ensure t)

(defun vr--use-whole-buffer ()
  (unless (region-active-p) (setq vr--target-buffer-start (point-min))))
;; (advice-add 'vr--set-target-buffer-start-end :after 'vr--use-whole-buffer)

(use-package which-key
    :init (which-key-mode 1)
    :ensure t)

(use-package xclip
    :ensure t
    :config (xclip-mode 1))

(use-package clipetty
    :ensure t
    :hook (after-init . global-clipetty-mode))

(use-package lua-mode
    :ensure t
    :config (setq lua-indent-level 4))

(use-package exec-path-from-shell
    :if (memq window-system '(mac ns))
    :ensure t
    :config
    (exec-path-from-shell-initialize)
    (when (memq window-system '(mac ns))
      (setenv "PATH" (concat "/opt/homebrew/bin/:" (getenv "PATH")))))

(use-package flx
    :ensure t)

(use-package wgrep
    :ensure t)

(use-package markdown-mode
    :ensure t
    :commands (markdown-mode gfm-mode)
    :mode (("README\\.md\\'" . gfm-mode)
           ("\\.md\\'"       . markdown-mode)
           ("\\.markdown\\'" . markdown-mode))
    :init (setq markdown-command "multimarkdown"))

(use-package magit
    :ensure t
    :bind (("C-x g" . magit-status)
           ("C-x C-g" . magit-status))
    :config
    (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)))

(use-package undo-tree
    :ensure t
    :config
    (setq undo-tree-auto-save-history t)
    (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/backup/undo-tree")))
    :init
    (global-undo-tree-mode))

(use-package paren
    :config
  (show-paren-mode +1))

;; (use-package company
;;     :ensure t
;;     :bind
;;     (:map company-active-map ("<return>" . nil))
;;     :hook
;;     (prog-mode . company-mode)
;;     :config
;;     (setq company-minimum-prefix-length 1
;;           company-idle-delay 0
;;           company-backends '((
;;                               company-files
;;                               company-dabbrev
;;                               company-etags
;;                               )))
;;     )

;; (use-package company-flx
;;     :ensure t)

;; (with-eval-after-load 'company
;;   (company-flx-mode +1))

;; (use-package company-box
;;     :ensure t)

(use-package racket-mode
    :ensure t
    :hook (racket-mode . racket-xp-mode))

(use-package go-mode
    :ensure t)

(use-package flycheck
    :disabled t
    :ensure t
    :bind (("<f2>" . flycheck-next-error)
           ("S-<f2>" . flycheck-previous-error)))

(use-package flycheck-clj-kondo
    :disabled t
    :ensure t)

(use-package paredit
    :ensure t
    :bind (("C-c (" . paredit-wrap-round)
           ("C-c {" . paredit-wrap-curly)
           ("C-c [" . paredit-wrap-square)
           ("C-c <" . paredit-wrap-angled)
           ("C-M-{" . backward-paragraph)
           ("C-M-}" . forward-paragraph))
    :init
    (progn
      (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
      (add-hook 'clojure-mode-hook 'paredit-mode)
      (add-hook 'clojurescript-mode-hook 'paredit-mode)
      (add-hook 'cider-repl-mode-hook 'paredit-mode)
      (add-hook 'racket-mode-hook 'paredit-mode)
      (add-hook 'racket-repl-mode-hook 'paredit-mode)
      (add-hook 'inferior-clojure-mode-hook 'paredit-mode)
      (add-hook 'scheme-mode-hook 'paredit-mode)
      (add-hook 'repl-mode-hook 'paredit-mode))
    :hook
    (sly-mode . paredit-mode)
    )

(use-package geiser-guile
    :ensure t
    :config
    (setq geiser-guile-load-init-file-p t))

(use-package clojure-mode
    :ensure t)

(use-package cider
    ;; :defer t
    :ensure t
    ;; :bind (("C-c =" . cider-format-buffer)
           ;; ("C-." . cider-find-dwim))
    :init
    (progn
      ;; (add-hook 'clojure-mode-hook 'cider-mode)
      (add-hook 'clojurescript-mode-hook 'cider-mode)
      ;; (add-hook 'clojurec-mode-hook 'cider-mode)
      (add-hook 'cider-repl-mode-hook 'cider-mode))
    :config
    (setq cider-repl-display-help-banner nil)
    (setq cider-auto-mode t)
    (setq show-paren-mode t)
    (setq cider-repl-pop-to-buffer-on-connect nil))

;;; gdb setup
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows nil
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t)

;; slynk doesn't compile with ECL
(use-package sly-quicklisp
  :after sly
  :ensure t)

(use-package sly
  :disabled t
  :ensure t
  :init
  (setq sly-net-coding-system 'utf-8-unix)
  (setq sly-lisp-implementations
        '((sbcl ("sbcl") :coding-system utf-8-unix)
          (ecl ("ecl") :coding-system utf-8-unix))))

(use-package fish-mode
    :ensure t)

(use-package dart-mode
    :ensure t
    :init
    (setq dart-format-on-save t))

(use-package yaml-mode
    :ensure t)

(use-package web-mode
    :ensure t)

;; drag and drop images into org mode
(use-package org-download
    :ensure t)

(use-package cargo
    :ensure t)

(use-package yasnippet
    :ensure t
    :diminish yas-minor-mode
    :bind (:map yas-minor-mode-map
                ("C-c C-e" . yas-expand))
    :config
    (yas-reload-all)
    (add-hook 'prog-mode-hook #'yas-minor-mode)
    ;;(yas-global-mode 1)
    (setq yas-prompt-functions '(yas-dropdown-prompt
                                 yas-ido-prompt
                                 yas-completing-prompt)))

(use-package expand-region
    :bind ("C-=" . er/expand-region))

(use-package license-templates
    :ensure t)

(use-package expand-region
    :ensure t)

(use-package zig-mode
    :ensure t)

(use-package multiple-cursors
    :ensure t)


(use-package tree-sitter
    :ensure t
    :config
    (global-tree-sitter-mode)
    (add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode))

(use-package tree-sitter-langs
    :ensure t)


(global-set-key (kbd "M-s") 'mc/edit-lines)

(use-package rainbow-mode
    :ensure t) ;; show color previews in buffers

;; TODO
(defun run-from-root ()
  (interactive)
  (message "finish later"))
;; ########################## Custom

;; (custom-theme-set-faces
;;  'almost-mono-black
;;  '(region ((t (:extend t :background "dim gray" :foreground "#ffffff")))))

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(company-dabbrev-downcase nil)
 '(compilation-message-face 'default)
 '(custom-enabled-themes '(standard-dark))
 '(eldoc-idle-delay 0)
 '(lisp-indent-function 'common-lisp-indent-function)
 '(package-selected-packages
   '(github-dark-vscode-theme sly-quicklisp tree-sitter-langs tree-sitter indent-guide standard-themes 0x0 company-box editorconfig xclip realgud-lldb 0blayout almost-mono-themes evil-surround evil-commentary diff-hl-mode global-diff-hl-mode message-view-patch slime geiser-chicken diff-hl elfeed avy ag color-theme-sanityinc-tomorrow visual-regexp simplicity-theme company-quickhelp-terminal simplicity flutter ef-themes clj-deps-new mood-one-theme skewer-mode tao-theme zenburn-theme simple-httpd livereload clojars atom-one-dark-theme web-server dracula-theme org-roam toml-mode go-mode monokai-theme gruvbox-theme evil counsel-fd magit flycheck-julia julia-repl eglot-jl janet-mode zig-mode yasnippet yaml-mode wgrep web-mode use-package undo-tree sublime-themes rustic rainbow-mode racket-mode protobuf-mode paredit org-download multiple-cursors monokai-pro-theme lua-mode license-templates ivy-clojuredocs geiser-guile flycheck-clj-kondo flx fish-mode expand-region exec-path-from-shell dart-mode counsel-at-point ompany cider cargo))
 '(smtpmail-smtp-server "smtp.gmail.com")
 '(smtpmail-smtp-service 25)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-very-old-color nil)
 '(vc-follow-symlinks t)
 '(warning-suppress-log-types '((comp)))
 '(window-divider-mode nil))


(put 'dired-find-alternate-file 'disabled nil)

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "JetBrains Mono" :foundry "JB" :slant normal :weight regular :height 101 :width normal))))
 '(region ((t (:extend t :background "dim gray" :foreground "#ffffff")))))

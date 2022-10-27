(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(require 'use-package)

;;; backup/autosave
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'control)
(setq-default cursor-type 'box)
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq recentf-max-saved-items 100)
(setq confirm-kill-emacs 'yes-or-no-p)
;;(setq visible-cursor nil) ;; don't blink in terminal
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

;; indent with spaces
(setq-default indent-tabs-mode nil)
(setq-default c-basic-offset 4)

;; indent with tabs
;; (setq-default indent-tabs-mode nil)
;; (defvaralias 'c-basic-offset 'tab-width)

(if (display-graphic-p)
    (scroll-bar-mode -1))

(define-key global-map (kbd "C-c r") 'vr/replace)
(define-key global-map (kbd "C-c q") 'vr/query-replace)
(define-key global-map (kbd "C-c m") 'vr/mc-mark)
(global-diff-hl-mode)

(add-hook 'c-mode-hook
          (lambda ()
            (setq-local fill-column 80)
            ;; (setq-local indent-tabs-mode t)
            (c-set-offset 'arglist-intro '+)
            (c-set-offset 'arglist-close 0)
            (display-fill-column-indicator-mode)))

(add-hook 'before-save-hook 'gofmt-before-save)

;;  "Recursively add all subdirectories of lisp dir to `load-path'.
(let ((default-directory  "~/.emacs.d/lisp/"))
  (normal-top-level-add-subdirs-to-load-path))

;; turning off until I remember what it does 
;; (setq c-offsets-alist '((arglist-cont-nonempty . +)))

(interactive "DDirectory: ")

(defun first-time-load ()
  (package-initialize)
  (package-refresh-contents)
  (package-install 'use-package))

(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

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

(defun change-theme (&rest args)
  "Like `load-theme', but disables all themes before loading the new one."
  ;; The `interactive' magic is for creating a future-proof passthrough (see <https://emacs.stackexchange.com/a/19242>).
  (interactive (advice-eval-interactive-spec
                (cadr (interactive-form #'load-theme))))
  (mapcar #'disable-theme custom-enabled-themes)
  (apply (if (called-interactively-p 'any) #'funcall-interactively #'funcall)
         #'load-theme args))

(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (setq-local indent-tabs-mode nil)))

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

(global-set-key (kbd "M-1") 'my/select-current-line-and-forward-line)

(defun user-racket-keybindings ()
  (local-set-key (kbd "C-c C-c") 'racket-run))

(add-hook 'racket-mode-hook #'user-racket-keybindings)

(defun user-flycheck-keybindings ()
  (local-set-key (kbd "S-<f2>") 'flycheck-previous-error)
  (local-set-key (kbd "<f2>") 'flycheck-next-error))

(add-hook 'flycheck-mode-hook		#'user-flycheck-keybindings)

(defun user-progmode-keybindings ()
  (local-set-key (kbd "C-c C-l") 'org-store-link))

(add-hook 'prog-mode-hook		#'user-progmode-keybindings)
;; problem exists below
(defun user-flymake-keybindings ()
  (local-set-key (kbd "S-<f2>") 'flymake-goto-prev-error)
  (local-set-key (kbd "<f2>") 'flymake-goto-next-error))
(add-hook 'flymake-mode-hook		#'user-flymake-keybindings)

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
(global-set-key (kbd "C-,") 'avy-goto-char)
(global-unset-key (kbd "C-<next>"))

(setq ido-enable-flex-matching t)
(setq ido-everywhere t)
(ido-mode 1)
(setq ido-file-extensions-order '(".org" ".c" ".go" ".el" ".lua" ".txt" ".py" ".emacs" ".xml" ".ini" ".cfg" ".cnf"))
(setq ido-use-virtual-buffers t)

;; i think this ignores annoying messages about people coming and going on IRC channels
(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(blink-cursor-mode 0)
(xterm-mouse-mode 1)

;; Escape C-x and C-c in terminal mode
(add-hook 'term-mode-hook (lambda ()
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

(use-package elfeed
    :ensure t
    :config
    (setq elfeed-feeds
          '(("http://nullprogram.com/feed/" blog emacs)
            ("https://eli.thegreenplace.net/feeds/all.atom.xml" blog dev)
            ("https://www.joelonsoftware.com/feed/" blog dev)
            ("https://danluu.com/atom.xml" dev blog)
            ("https://drewdevault.com/feed.xml" blog dev)
            )))

(use-package ag
    :ensure t)

(use-package avy
    :ensure t)

(use-package visual-regexp
    :ensure t)

;; problem exists above
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

(use-package company
  :ensure t
  :bind
  (:map company-active-map ("<return>" . nil))
  :hook
  (prog-mode . company-mode)
  :config
  (setq company-backends '((company-capf  company-files))))

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
  :bind (("C-c \(" . paredit-wrap-round)
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
  (sly-mode . paredit-mode))

(use-package geiser-guile
  :ensure t
  :config
  (setq geiser-guile-load-init-file-p t))

(use-package clojure-mode
  :ensure t)

(use-package cider
  ;; :defer t
  :ensure t
  :bind (("C-c =" . cider-format-buffer)
         ("C-." . cider-find-dwim))
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
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t)

;; load quicklisp packages with a hotkey
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

(use-package eglot
  :ensure t
  :init
  (progn
    (add-hook 'rust-mode-hook 'eglot-ensure)
    (add-hook 'go-mode-hook 'eglot-ensure)
    (add-hook 'c-mode-hook 'eglot-ensure)
    (add-hook 'c++-mode-hook 'eglot-ensure)
    (add-hook 'dart-mode-hook 'eglot-ensure)))



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

(global-set-key (kbd "M-s") 'mc/edit-lines)

(use-package rainbow-mode
  :ensure t) ;; show color previews in buffers

(setq rustic-lsp-client 'eglot)

;; TODO
(defun run-from-root ()
  (interactive)
  (message "finish later"))
;; ########################## Custom

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(company-quickhelp-delay 0)
 '(company-quickhelp-mode t)
 '(compilation-message-face 'default)
 '(eldoc-idle-delay 0)
 '(lisp-indent-function 'common-lisp-indent-function)
 '(package-selected-packages
   '(geiser-chicken diff-hl elfeed avy ag color-theme-sanityinc-tomorrow visual-regexp simplicity-theme company-quickhelp-terminal simplicity flutter ef-themes clj-deps-new mood-one-theme skewer-mode tao-theme zenburn-theme simple-httpd livereload clojars atom-one-dark-theme web-server dracula-theme org-roam plan9-theme toml-mode go-mode xclip monokai-theme gruvbox-theme evil counsel-fd magit flycheck-julia julia-repl eglot-jl janet-mode zig-mode yasnippet yaml-mode wgrep web-mode use-package undo-tree sublime-themes sly-quicklisp rustic rainbow-mode racket-mode protobuf-mode paredit org-download naysayer-theme multiple-cursors monokai-pro-theme lua-mode license-templates ivy-clojuredocs geiser-guile flycheck-clj-kondo flx fish-mode expand-region exec-path-from-shell dart-mode counsel-at-point company cider cargo))
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
 '(default ((t (:background nil)))))



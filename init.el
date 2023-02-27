(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               ;; restore after startup
                               (setq gc-cons-threshold 800000)))
(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(require 'use-package)
(require 'recentf)
(require 'dired)

;; ;;; backup/autosave
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))

(setq mac-option-modifier 'meta)
(setq mac-command-modifier 'control)
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq set-mark-command-repeat-pop t) ;; cycle thru marks w/ c-space
(setq mouse-wheel-progressive-speed nil) ;; turn off mouse acceleration
(setq read-file-name-completion-ignore-case t) ;;; case insenstive autocompletion
;; (setq eldoc-echo-area-use-multiline-p nil)
;; (setq-default show-trailing-whitespace t)
(setq dired-kill-when-opening-new-dired-buffer t) ;; stop dired from cluttering buffer list
(setq c-default-style "k&r")
(setq create-lockfiles nil)
(setq minibuffer-prompt-properties
      '(read-only t cursor-intangible t face minibuffer-prompt))
;; indent with spaces
(setq-default indent-tabs-mode nil)
(setq c-basic-offset 4)
(setq dired-deletion-confirmer '(lambda (x) t))
(setq split-height-threshold nil) ;; only open horizontal splits (works on searches)
(setq undo-limit 20000000)
(setq undo-strong-limit 40000000)
(setq inhibit-startup-screen t)
;;; gdb setup
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows nil
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t)
(setq esup-depth 0) ;; for profiling
(setq recentf-max-menu-items 50)

(if (display-graphic-p)
    (scroll-bar-mode -1))
(electric-pair-mode 1)
(recentf-mode 1)

(add-hook 'html-mode-hook
          (lambda ()
            (setq c-basic-offset 2)))
(add-hook 'c-mode-hook
          (lambda ()
            (setq-local fill-column 80)
            (c-set-offset 'arglist-intro '+)
            (c-set-offset 'arglist-close 0)))
(add-hook 'write-file-functions 'delete-trailing-whitespace)
(add-hook 'prog-mode-hook		#'user-progmode-keybindings)
(add-hook 'flymake-mode-hook
          (lambda ()
            (local-set-key (kbd "S-<f2>") 'flymake-goto-prev-error)
            (local-set-key (kbd "<f2>") 'flymake-goto-next-error)))

(defun first-time-load ()
  (package-initialize)
  (package-refresh-contents)
  (package-install 'use-package))

(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun mark-advance-line (arg)
  "Select the current line and move the cursor by ARG lines IF
no region is selected.

If a region is already selected when calling this command, only move
the cursor by ARG lines."
  (interactive "p")
  (when (not (use-region-p))
    (forward-line 0)
    (set-mark-command nil))
  (forward-line arg))

(defun launch-iterm-in-vc-root ()
  (interactive)
  (let ((project-root (cdr (project-current))))
    (shell-command (concat "open -a iTerm " project-root) nil nil)))

(defun user-progmode-keybindings ()
  (local-set-key (kbd "C-c C-l") 'org-store-link))

;;; I like to take notes in text files where I refer to stuff with filename:line w/o org mode
(defun yank-filename-and-line ()
  "Yank the current filename and line number at point to the kill ring."
  (interactive)
  (let ((filename (buffer-file-name))
        (line (line-number-at-pos)))
    (when filename
      (kill-new (format "%s:%d" filename line))
      (message "Yanked: %s:%d" filename line))))

(defun visit-source ()
  "If the current line contains text like './src/program.rb:34:',
visit that file in the other window and position point on that
line. A file must either have a / or . in the filename to be
recognized."
  "   # ./app/views/interfaces/edit.html.erb:3:fdsin"
  "   # ./app/views/interfaces/edit.html.erb:3:in"
  "   # ./app/views/interfaces/edit.html.erb:3"
  "   # ./app:3"
  "   # bar/foo"
  (interactive)
  (let* ((path (if (use-region-p)
                   (buffer-substring-no-properties (region-beginning) (region-end))
                 (let (p0 p1 p2)
                   (setq p0 (point))
                   ;; chars that are likely to be delimiters of full path, e.g. space, tabs, brakets.
                   (skip-chars-backward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\`")
                   (setq p1 (point))
                   (goto-char p0)
                   (skip-chars-forward "^  \"\t\n`'|()[]{}<>〔〕“”〈〉《》【】〖〗«»‹›·。\\'")
                   (setq p2 (point))
                   (goto-char p0)
                   (buffer-substring-no-properties p1 p2))))

         (match (and path (string-match "^\\([^: ]*[/.][^: ]*\\)\\(:[0-9]+\\)?\\(:[0-9]+\\)?:?" path)))
         (fpath (if match (match-string 1 path) path))
         (line-no (if (match-string 2 path) (string-to-number (substring (match-string 2 path) 1)) 0))
         (col-no (if (match-string 3 path) (string-to-number (substring (match-string 3 path) 1)) 0)))
    (if match
        (progn
          (find-file fpath)
          (when line-no
            ;; goto-line is only for interactive use
            (goto-char (point-min))
            (forward-line (1- line-no))
            (when (> col-no 0)
              (forward-char (1- col-no)))))
      (error "No source location on line."))))

(defun copy-parent-dir-as-kill ()
  (interactive)
  (kill-new (expand-file-name default-directory)))

(define-key global-map (kbd "C-z") nil)
(define-key global-map (kbd "C-x C-l") nil)
(define-key global-map (kbd "C-<next>") nil)
(define-key global-map "\eo" #'previous-buffer)
(define-key global-map (kbd "C-M-o") #'delete-other-windows)
(define-key global-map "" 'other-window)
(define-key global-map (kbd "C-;") #'comment-line)
(define-key global-map [f2] nil)
(define-key global-map (kbd "<next>") 'View-scroll-half-page-forward)
(define-key global-map (kbd "<prior>") 'View-scroll-half-page-backward)
(define-key global-map (kbd "C-,") 'project-find-regexp)
(define-key global-map (kbd "C-\\") 'consult-buffer)
(define-key global-map (kbd "C-x b") 'switch-to-buffer)
(define-key global-map "\ep" 'exchange-point-and-mark)
(define-key global-map "\e`" #'other-frame)
(define-key global-map (kbd "<C-left>") #'back-to-indentation)
(define-key global-map (kbd "<C-right>") #'move-end-of-line)
(define-key global-map (kbd "C-c C-f") 'format-all-buffer)
(define-key global-map (kbd "C-c r") 'project-compile)
(define-key global-map "\e1" 'mark-advance-line)
(define-key global-map "\e2" 'mark-defun)
(define-key global-map "\e3" 'imenu)
(define-key global-map "\es" 'cua-rectangle-mark-mode)
(define-key global-map "\e/" #'yank-filename-and-line)
(define-key global-map (kbd "C-.") 'visit-source)
(define-key global-map "\C-x\ \C-r" 'recentf-open-files)

(define-key emacs-lisp-mode-map (kbd "<f5>") 'eval-buffer)
(define-key dired-mode-map "o" nil)
(define-key dired-mode-map "" nil)

(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))

(blink-cursor-mode 0)
(xterm-mouse-mode 1)
(tool-bar-mode -1)
(menu-bar-mode -1)

;; ;; Escape C-x and C-c in terminal mode
;; (add-hook 'term-mode-hook
;;           (lambda ()
;;             ;; Hack to set two escape chars.
;;             (let (term-escape-char)
;;               (term-set-escape-char ?\C-x))
;;             (let (term-escape-char)
;;               (term-set-escape-char ?\C-c))))

(add-hook 'flymake-mode-hook
          (lambda ()
            (local-set-key (kbd "M-n") 'flymake-goto-next-error)
            (local-set-key (kbd "M-p") 'flymake-goto-prev-error)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
             (local-set-key (kbd "C-c C-c") 'eval-buffer)))
(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-r") #'compile)))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)


(use-package savehist
  :init
  (savehist-mode))

;; (use-package format-all
;;   :ensure t)

(use-package xclip
  :ensure t
  :config (xclip-mode 1))

(use-package magit
  :disabled t
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status))
  :config
  (setq magit-log-margin '(t "%Y-%m-%d %H:%M " magit-log-margin-width t 18)))

(use-package undo-fu
  :ensure t
  :config
  (define-key global-map (kbd "C-z") nil)
  (global-set-key (kbd "C-z")   'undo-fu-only-undo)
  (global-set-key (kbd "C-S-z") 'undo-fu-only-redo))

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
  (sly-mode . paredit-mode))

;; (use-package clojure-mode
;;   :ensure t)

;; (use-package cider
;;   :ensure t
;;     :config
;;     (setq cider-repl-display-help-banner nil)
;;     (setq cider-auto-mode t)
;;     (setq show-paren-mode t)
;;     (setq cider-repl-pop-to-buffer-on-connect nil))

;; Startup time
(defun display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

(add-hook 'emacs-startup-hook #'display-startup-time)
(global-hl-line-mode 1)
(set-face-background 'hl-line "midnight blue")

(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-enabled-themes nil)
 '(custom-safe-themes
   '("8b930a6af47e826c12be96de5c28f1d142dccab1927f196589dafffad0fc9652" "5d59bd44c5a875566348fa44ee01c98c1d72369dc531c1c5458b0864841f887c" "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" "19a2c0b92a6aa1580f1be2deb7b8a8e3a4857b6c6ccf522d00547878837267e7" "cb4c6fef7d911b858f907f0c93890c4a44a78ad22537e9707c184f7e686e8024" "5a45c8bf60607dfa077b3e23edfb8df0f37c4759356682adf7ab762ba6b10600" "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2" "279f74e365ba5aade8bc702e0588f0c90b5dee6cf04cf61f9455661700a6ebeb" "9fad628c15f1e94af44e07b00ebe3c15109be28f4d73adf4a9e22090845cbce9" default))
 '(initial-frame-alist '((fullscreen . maximized)))
 '(package-selected-packages '(undo-fu xclip use-package paredit magit consult)))

(define-key global-map "\t" 'dabbrev-expand)
(define-key global-map [S-tab] 'indent-for-tab-command)

(add-to-list 'default-frame-alist '(font . "Iosevka-16"))
(set-face-attribute 'default t :font "Iosevka-16")
(set-face-attribute 'font-lock-builtin-face nil :foreground "#DAB98F")
(set-face-attribute 'font-lock-comment-face nil :foreground "gray50")
(set-face-attribute 'font-lock-constant-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-doc-face nil :foreground "gray50")
(set-face-attribute 'font-lock-function-name-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-keyword-face nil :foreground "DarkGoldenrod3")
(set-face-attribute 'font-lock-string-face nil :foreground "olive drab")
(set-face-attribute 'font-lock-type-face nil :foreground "burlywood3")
(set-face-attribute 'font-lock-variable-name-face nil :foreground "burlywood3")
(set-foreground-color "burlywood3")
(set-background-color "#161616")
(set-cursor-color "#40FF40")
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 )

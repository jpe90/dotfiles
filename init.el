;; === Core Settings ===
(if (eq system-type 'darwin)
    (progn
      (set-face-attribute 'default nil :font "Iosevka" :height 140)
      (menu-bar-mode 1)))

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; Core requires
(require 'recentf)
(require 'uniquify)
(require 'view)

;; === Basic Configuration ===
(defalias 'yes-or-no-p 'y-or-n-p)
(autoload 'zap-up-to-char "misc" "Kill up to, but not including ARGth occurrence of CHAR." t)

;; C++ configuration
(setq-default c-basic-offset 4)
(setq c-default-style "k&r")

(setq
      mac-option-modifier 'meta
      mac-command-modifier 'ctrl
      set-mark-command-repeat-pop t
      mouse-wheel-progressive-speed nil
      read-file-name-completion-ignore-case t
      dired-kill-when-opening-new-dired-buffer t
      c-default-style "k&r"
      create-lockfiles nil
      minibuffer-prompt-properties '(read-only t cursor-intangible t face minibuffer-prompt)
      c-basic-offset 4
      dired-deletion-confirmer '(lambda (x) t)
      split-height-threshold nil
      undo-limit 20000000
      undo-strong-limit 40000000
      tags-revert-without-query t
      edebug-print-length nil
      edebug-print-level nil
      use-short-answers t
      mac-pass-command-to-system nil
      mac-pass-control-to-system nil
      recentf-max-menu-items 500
      recentf-max-saved-items 500
      save-interprogram-paste-before-kill t
      apropos-do-all t
      mouse-yank-at-point t
      require-final-newline t
      visible-bell nil
      load-prefer-newer t
      backup-by-copying t
      frame-inhibit-implied-resize t
      ediff-window-setup-function 'ediff-setup-windows-plain
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      inhibit-startup-screen t
      dired-mouse-drag-files t
      gdb-many-windows t
      gdb-show-main t
      compilation-ask-about-save nil
      tao-theme-use-boxes nil
      tao-theme-use-sepia nil
      compilation-always-kill t
      completion-cycle-threshold 10
      modus-themes-fringes nil
      save-interprogram-paste-before-kill t
      compilation-scroll-output t
      inferior-lisp-program "sbcl"
      completion-styles '(flex)
      ;; put backup files in the .emacs.d/backups directory
      backup-directory-alist `(("." . ,(concat user-emacs-directory
                                               "backups")))
      doom-gruvbox-dark-variant "hard"
      ;; gptel-default-mode 'org-mode
      )
(use-package undo-tree
  :diminish ;; Don't show an icon in the modeline
  :ensure t
  :bind ("C-x u" . undo-tree-visualize)
  :init
  (global-undo-tree-mode)
  :config
  ;; Each node in the undo tree should have a timestamp.
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo"))))

(use-package smex
  :ensure t
  :init
  (smex-initialize))

(use-package which-key
  :ensure t)

;; PREREQUISITE: git
(use-package magit
  :ensure t)

;; PREREQUISITE: rg
(use-package deadgrep
  :ensure t)

(use-package multiple-cursors
  :ensure t
  :bind (
	( "C-S-c C-S-c" . 'mc/edit-lines)
	("C-S-a C-S-a" . 'mc/edit-beginnings-of-lines)
	("C-c C->" . 'mc/mark-all-like-this)
	("C->" . 'mc/mark-next-like-this)
	:map multiple-cursors-mode
	(("C-?" . 'mc/unmark-next-like-this)
	 ("C-c C-?".'mc/skip-to-next-like-this)
	 ("C-<" . 'mc/mark-previous-like-this)
	 ("C-S-s" . 'phi-search)
	 ("C-S-r" . 'phi-search-backward))))

;; === Mode Enabling/Disabling ===
(delete-selection-mode 1)
(recentf-mode 1)
(global-auto-revert-mode 1)
(savehist-mode 1)
(save-place-mode 1)
(show-paren-mode 1)
(ido-mode 'buffers)
;; (menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)
(pixel-scroll-mode 1)

;; ;; === Package Configuration ===

;; ido + smex
(ido-mode t)
(setq ido-enable-flex-matching t)

(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

;; === Functions ===

(defun previous-buffer ()
  "Switch to previously open buffer.
Repeated  toggle between the two most recently open buffers."
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

;; ==== stuff with rc prefix comes from Alexey Kutepov  https://github.com/rexim/dotfiles

(defun rc/buffer-file-name ()
  (if (equal major-mode 'dired-mode)
      default-directory
    (buffer-file-name)))

;;; Taken from here:
;;; http://stackoverflow.com/questions/2416655/file-path-to-clipboard-in-emacs
(defun rc/put-file-name-on-clipboard ()
  "Put the current file name on the clipboard"
  (interactive)
  (let ((filename (rc/buffer-file-name)))
    (when filename
      (kill-new filename)
      (message filename))))

(defun rc/put-buffer-name-on-clipboard ()
  "Put the current buffer name on the clipboard"
  (interactive)
  (kill-new (buffer-name))
  (message (buffer-name)))

(defun rc/parent-directory (path)
  (file-name-directory (directory-file-name path)))

(defun rc/put-parent-directory-on-clipboard ()
  "Put the current buffer's parent directory on the clipboard"
  (interactive)
  (let ((parent-directory (rc/parent-directory (rc/buffer-file-name))))
    (kill-new parent-directory)
    (message parent-directory)))

;;; Stolen from http://ergoemacs.org/emacs/emacs_unfill-paragraph.html
(defun rc/unfill-paragraph ()
  "Replace newline chars in current paragraph by single spaces.
This command does the inverse of `fill-paragraph'."
  (interactive)
  (let ((fill-column 90002000)) ; 90002000 is just random. you can use `most-positive-fixnum'
    (fill-paragraph nil)))

(global-set-key (kbd "C-c M-q") 'rc/unfill-paragraph)

(defun rc/load-path-here ()
  (interactive)
  (add-to-list 'load-path default-directory))

(defun rc/duplicate-line ()
  "Duplicate current line"
  (interactive)
  (let ((column (- (point) (point-at-bol)))
        (line (let ((s (thing-at-point 'line t)))
                (if s (string-remove-suffix "\n" s) ""))))
    (move-end-of-line 1)
    (newline)
    (insert line)
    (move-beginning-of-line 1)
    (forward-char column)))

(global-set-key (kbd "C-,") 'rc/duplicate-line)

;; === Keybindings ===

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-;") 'comment-line)
(global-set-key (kbd "C-<next>") nil)
(global-set-key (kbd "C-M-o") 'delete-other-windows)
(global-set-key (kbd "C-c b") 'switch-to-buffer)
(global-set-key (kbd "<C-left>") 'back-to-indentation)
(global-set-key (kbd "<C-right>") 'move-end-of-line)
(global-set-key (kbd "C-c p") 'project-find-file)
(global-set-key (kbd "C-o") 'other-window)
(global-set-key (kbd "M-o") 'previous-buffer)
(global-set-key (kbd "C-`") 'other-frame)
(global-set-key (kbd "C-x l") 'recentf)
(global-set-key (kbd "C-x C-l") 'recentf-open-files)
(global-set-key (kbd "M-1") 'mark-advance-line)
(global-set-key (kbd "M-2") 'mark-defun)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-c M-l") 'org-store-link)
(global-set-key (kbd "C-c g") 'deadgrep)
(global-set-key (kbd "C-c @") 'mark-sexp)
(global-set-key (kbd "C-S-s") 'isearch-forward-thing-at-point)

(global-set-key (kbd "C-ф") 'forward-char)
(global-set-key (kbd "C-б") 'backward-char)
(global-set-key (kbd "C-н") 'next-line)
(global-set-key (kbd "C-п") 'previous-line)
(global-set-key (kbd "M-в") 'scroll-up-command)
(global-set-key (kbd "C-в") 'scroll-down-command)
(global-set-key (kbd "C-к") 'kill-line)
(global-set-key (kbd "C-д") 'delete-char)
(global-set-key (kbd "M-д") 'kill-word)
(global-set-key (kbd "C-а") 'beginning-of-line)
(global-set-key (kbd "C-е") 'end-of-line)
(global-set-key (kbd "C-с") 'isearch-forward)
(global-set-key (kbd "C-р") 'isearch-backward)
(global-set-key (kbd "C-х б") 'switch-to-buffer)
(global-set-key (kbd "C-х л") 'recentf)
(global-set-key (kbd "C-х г") 'keyboard-quit)

(define-key global-map (kbd "C-x t") 'beginning-of-buffer)
(define-key global-map (kbd "C-x e") 'end-of-buffer)

(global-set-key [S-tab] 'indent-for-tab-command)

;; I split out some system or setup specific settings into separate files, so I
;; have some code to look for those and only load them if they exist

(when (file-directory-p "~/.emacs.d/lisp")
  (add-to-list 'load-path "~/.emacs.d/lisp")

  (when (file-directory-p "~/.emacs.d/lisp/witness")
    (add-to-list 'load-path "~/.emacs.d/lisp/witness")))

(when (file-readable-p "~/.emacs.d/custom.el")
  (load "~/.emacs.d/custom.el"))

(when (file-exists-p "/Users/jon/.emacs.d/local-config.el")
  (load-file "/Users/jon/.emacs.d/local-config.el"))

(defun disable-font-lock-in-compilation-buffer ()
  "Disable syntax highlighting in the compilation buffer."
  (font-lock-mode -1))

(add-hook 'compilation-mode-hook 'disable-font-lock-in-compilation-buffer)

(with-eval-after-load 'multiple-cursors
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click))

;; switch between header and source file for c/h or cpp/h or cc/hh
(defun switch-header-source ()
  "Switch between header and source file."
  (interactive)
  (let* ((file-name (buffer-file-name))
         (file-name-sans-extension (file-name-sans-extension file-name))
         (extension (file-name-extension file-name))
         (header-extension (cond
                            ((string= extension "h") "cpp")
                            ((string= extension "hpp") "cpp")
                            ((string= extension "c") "h")
                            ((string= extension "cc") "hh")
                            ((string= extension "cpp") "h")
                            ((string= extension "hh") "cc")
                            (t nil)))
         (header-file-name (concat file-name-sans-extension "." header-extension)))
    (if (file-exists-p header-file-name)
        (find-file header-file-name)
      (message "Header file not found."))))

(global-set-key (kbd "<f8>") 'switch-header-source)

(defun disable-eldoc-in-python-mode ()
  "Disable eldoc-mode in python-mode."
  (when (derived-mode-p 'python-mode)
    (eldoc-mode -1)))
(defun my-cmake-compile ()
  "run make out of the build subdirectory of the project root"
  (interactive)
    (let ((default-directory (concat (project-root (project-current t)) "/build"))
            (command "cmake --build . --config Debug"))
      (compile command)))



;; (when (file-exists-p "/Users/jon/.emacs.d/setup-meow.el")
;;   (load-file "/Users/jon/.emacs.d/setup-meow.el"))


;; (use-package evil
;;  :ensure t
;;  :init
;;  (setq evil-want-C-u-scroll t)
;;  (setq evil-undo-system 'undo-tree)
;;  :config
;;  (evil-mode 1))

;; (global-display-line-numbers-mode 1)
;; (setq display-line-numbers-type 'relative)

;; (use-package evil-commentary
;;  :ensure t
;;  :config
;;  (evil-commentary-mode 1))

;; (require 'powershell-ts-mode)

(defun org-copy-src-block ()
  (interactive)
  (org-edit-src-code)
  (mark-whole-buffer)
  (kill-ring-save (point-min) (point-max))
  (org-edit-src-exit))


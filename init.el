;; === Packages and Initialization ===

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'recentf)
(require 'uniquify)
(require 'view)

(use-package rg
  :ensure t)

(use-package xref
  :init
  (setq xref-search-program (cond
                             ((executable-find "ugrep") 'ugrep)
                             ((executable-find "rg") 'ripgrep)
                             (t 'grep))))

(use-package magit
  :ensure t)

(use-package smex
  :ensure t
  :demand
  :config
  (global-set-key (kbd "M-x") 'smex)
  (global-set-key (kbd "M-X") 'smex-major-mode-commands)
  (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   '("," . xref-pop-marker-stack)
   '("." . xref-find-definitions)
   '("f" . project-find-file)
   '("w" . other-window)
   '("W" . window-swap-states)
   '("o" . delete-other-windows)
   '("s" . split-window-right)
   '("-" . split-window-below)
   '("&" . +change-theme)
   '(";" . comment-line)
   '("K" . kill-this-buffer)
   '("d" . dired)
   '("b" . switch-to-buffer)
   '("r" . rg-project)
   '("f" . find-file)
   '("i" . imenu)
   '("a" . execute-extended-command)
   '("=" . org-store-link)
   '("p" . project-find-file)
   '("j" . project-switch-to-buffer)
   '("t" . tab-bar-switch-to-tab)
   '("l" . project-switch-project)	
   '("y" . magit)
   '("l" . recentf-open-files)
   '("n" . org-roam-keymap)
   '(":" . eval-expression)
   '("e" . "C-x C-e")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("s" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("j" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("k" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("&" . meow-query-replace-regexp)
   '("%" . meow-query-replace)
   '("]" . scroll-up-command)
   '("[" . scroll-down-command)
   '("{" . backward-paragraph)
   '("}" . forward-paragraph)
   '("<escape>" . ignore)))

(use-package meow
  :ensure t
  :config
  (meow-setup)
  (meow-global-mode 1)
  (setq meow-use-clipboard t)
  (setq meow-visit-sanitize-completion nil)
  )  
  
(use-package xclip
  :ensure t)

(use-package vundo
  :ensure t)

(use-package kaolin-themes
  :ensure t
  :config
  (load-theme 'kaolin-dark t))

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; === Settings and Modes ===

;; Various Settings
(setq mac-option-modifier 'meta
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
      visible-bell t
      load-prefer-newer t
      backup-by-copying t
      frame-inhibit-implied-resize t
      ediff-window-setup-function 'ediff-setup-windows-plain
      custom-file (expand-file-name "custom.el" user-emacs-directory))

(unless backup-directory-alist
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups")))))

;; Modes
(delete-selection-mode 1)
(xclip-mode 1)
(xterm-mouse-mode 1)
(recentf-mode 1)
(global-auto-revert-mode 1)
(ido-everywhere)
(show-paren-mode 1)
(savehist-mode 1)
(setq-default indent-tabs-mode nil)
(save-place-mode 1)

;; === Functions ===

(defun previous-buffer ()
  "Switch to previously open buffer.
Repeated  toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

;; === Keybindings ===

(global-set-key (kbd "M-/") 'hippie-expand)
(global-set-key (kbd "C-x C-b") 'ibuffer)
(global-set-key (kbd "M-z") 'zap-up-to-char)
(global-set-key (kbd "C-s") 'isearch-forward-regexp)
(global-set-key (kbd "C-r") 'isearch-backward-regexp)
(global-set-key (kbd "C-M-s") 'isearch-forward)
(global-set-key (kbd "C-M-r") 'isearch-backward)
(global-set-key (kbd "C-z") nil)
(global-set-key (kbd "C-<next>") nil)
(global-set-key (kbd "C-M-o") 'delete-other-windows)
(global-set-key (kbd "C-c b") 'switch-to-buffer)
(global-set-key (kbd "<C-left>") 'back-to-indentation)
(global-set-key (kbd "<C-right>") 'move-end-of-line)
(global-set-key (kbd "C-c p") 'project-find-file)
(global-set-key (kbd "M-o") 'previous-buffer)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key [S-tab] 'indent-for-tab-command)
(define-key Info-mode-map [remap scroll-up-command] 'View-scroll-half-page-forward)
(define-key Info-mode-map [remap scroll-down-command] 'View-scroll-half-page-backward)

;; Load Custom File
(load "~/.emacs.d/custom.el")


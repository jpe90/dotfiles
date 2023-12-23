;; === Packages and Initialization ===

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'recentf)
(require 'uniquify)
(require 'view)

(use-package deadgrep
  :ensure t)

(use-package magit
  :disabled t)

(use-package xclip
  :disabled t)

(use-package undo-tree
  :diminish                       ;; Don't show an icon in the modeline
  :ensure t
  :bind ("C-x u" . undo-tree-visualize)
  :init
  (global-undo-tree-mode)
  :config
    ;; Each node in the undo tree should have a timestamp.
  (setq undo-tree-visualizer-timestamps t)
  (setq undo-tree-history-directory-alist '(("." . "~/.emacs.d/undo")))

    ;; Show a diff window displaying changes between undo nodes.
    (setq undo-tree-visualizer-diff t))

(defalias 'yes-or-no-p 'y-or-n-p)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; === Various Settings ===

(setq mac-option-modifier 'meta
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
      visible-bell t
      load-prefer-newer t
      backup-by-copying t
      frame-inhibit-implied-resize t
      ediff-window-setup-function 'ediff-setup-windows-plain
      custom-file (expand-file-name "custom.el" user-emacs-directory)
      inhibit-startup-screen t
      dired-mouse-drag-files t
      gdb-many-windows t
      gdb-show-main t)



(unless backup-directory-alist
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups")))))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; === Modes ===

(delete-selection-mode 1)
;; (xclip-mode 1)
;; (xterm-mouse-mode 1)
(recentf-mode 1)
(global-auto-revert-mode 1)
(show-paren-mode 1)
(savehist-mode 1)
(save-place-mode 1)
;; (global-hl-line-mode 1)
(blink-cursor-mode -1)
;; (scroll-bar-mode -1)
;; (context-menu-mode 1)
(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)

;; === Functions ===

(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command
   (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name))))

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

;; ==== random stuff stolen from tsoding https://github.com/rexim/dotfiles

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
(global-set-key (kbd "M-`") 'other-frame)
(global-set-key (kbd "C-x C-l") 'recentf-open-files)
(global-set-key (kbd "M-1") 'mark-advance-line)
(global-set-key (kbd "M-2") 'mark-defun)
(global-set-key (kbd "M-n") 'forward-paragraph)
(global-set-key (kbd "M-p") 'backward-paragraph)
(global-set-key (kbd "C-c M-l") 'org-store-link)
(global-set-key (kbd "C-c g") 'deadgrep)

(define-key global-map (kbd "C-x t") 'beginning-of-buffer)
(define-key global-map (kbd "C-x e") 'end-of-buffer)
(global-set-key [S-tab] 'indent-for-tab-command)
(define-key Info-mode-map [remap scroll-up-command] 'View-scroll-half-page-forward)
(define-key Info-mode-map [remap scroll-down-command] 'View-scroll-half-page-backward)

;; ======== trial usage

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

;; tsoding jai mode
;; (require 'jai-mode)
;; (add-to-list 'auto-mode-alist '("\\.jai\\'" . jai-mode))

;; (use-package company
;;   :ensure t
;;   :init
;;   (global-company-mode))

(add-hook 'c++-mode-hook 'ggtags-mode)

(add-hook 'org-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c w") 'rc/org-link-copy)))

(defun my-project-compile ()
  "Run `compile' in the project root using the last entered command."
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (compilation-buffer-name-function
         (or project-compilation-buffer-name-function
             compilation-buffer-name-function))
        (command (or (and (boundp 'compile-command) compile-command)
                     "make")))
    (compile command)))

;; ;; (setq my-current-project "/home/solaire/development/devkitpro/switch-examples/graphics/sdl2/sdl2-sanitycheck")
;; (defun my-project-compile ()
;;   "Run compile using `my-current-project` as the base directory if set, otherwise in the project root."
;;   (interactive)
;;   (let ((default-directory (if (and (boundp 'my-current-project)
;;                                     my-current-project
;;                                     (not (string-empty-p my-current-project)))
;;                                my-current-project
;;                              (project-root (project-current t))))
;;         (command (or (and (boundp 'compile-command) compile-command)
;;                      "make")))
;;     (compile command)))


(defun my-compile ()
  "Run compile without prompt."
  (interactive)
  ;; Use the existing compile-command, or "make" if compile-command is empty
  (let ((command (if (and compile-command (not (string-empty-p compile-command)))
                     compile-command
                   "make")))
    (compile command)))


(global-set-key (kbd "<f5>") 'project-compile)
(global-set-key (kbd "<f6>") (lambda () (interactive) (shell-command "/home/solaire/development/cpp/egl_opengl_study/build/03_cube_with_texture")))
(global-set-key (kbd "<f7>") 'clang-format-buffer)

(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless flex)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

(defun jai-compile-file ()
  "Compile the current Jai buffer."
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
         (base-name (file-name-sans-extension file-name))
         (compile-command (format "jai-macos %s.jai && ./%s" base-name base-name)))
    (compile compile-command)))

(defun python-compile-file ()
    "Compile the current Python buffer."
    (interactive)
    (let* ((file-name (file-name-nondirectory (buffer-file-name)))
             (base-name (file-name-sans-extension file-name))
             (compile-command (format "python3 %s.py" base-name)))
        (compile compile-command)))

(defun my/copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (dabbrev-expand)))

(defun my/copilot-c-g ()
  (interactive)
  (or (copilot-clear-overlay)
      (keyboard-quit)))

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package rust-mode
  :ensure t)

(with-eval-after-load 'copilot
  (define-key copilot-mode-map (kbd "C-<tab>") #'copilot-accept-completion)
  (define-key copilot-mode-map (kbd "C-g") #'my/copilot-c-g))


(when (file-readable-p "~/.emacs.d/custom.el")
  (load "~/.emacs.d/custom.el"))

;; (add-to-list 'load-path "/Users/jon/.emacs.d/lisp/copilot.el")
;; (require 'copilot)

(defun dired-do-llm-format (&optional arg)
  "Create a buffer with the contents of the marked (or next ARG) files, formatted in Markdown."
  (interactive)
  (let ((file-list (dired-get-marked-files t arg nil nil t))
        (buffer (generate-new-buffer "*LLM Format*")))
    (with-current-buffer buffer
      (dolist (file file-list)
        ;; Insert the file name with backticks
        (insert "```" (file-name-nondirectory file) "\n")
        ;; Save the position
        (let ((start (point)))
          ;; Insert the file contents
          (insert-file-contents file)
          ;; Insert the closing backticks
          (goto-char (point-max))
          (insert "\n```\n")))
      (goto-char (point-min)))
    (switch-to-buffer buffer)))

(defun create-llm-compilation-output ()
  "Create a buffer named *LLM Compilation Output* with specified content."
  (interactive)
  (let ((compilation-content (with-current-buffer "*compilation*"
                               (buffer-string)))
        (current-content (buffer-string))
        (current-filename (buffer-file-name)))

    ;; Create new buffer
    (with-current-buffer (get-buffer-create "*LLM Compilation Output*")
      ;; Insert compilation content
      (insert compilation-content "\n\n")

      ;; Insert current buffer content enclosed in markdown code ticks
      (insert "```" (file-name-nondirectory current-filename) "\n"
              current-content
              "```\n")

      ;; Display the buffer
      (switch-to-buffer-other-window "*LLM Compilation Output*"))))

(with-eval-after-load 'dired
  (define-key dired-mode-map (kbd "b") 'dired-do-llm-format))


;; ======================== not currently in use

;; === cua ===

;; (cua-mode 1)
;; (global-set-key (kbd "<M-up>") 'scroll-down-command)
;; (global-set-key (kbd "<M-down>") 'scroll-up-command)
;; (global-set-key (kbd "C-a") 'mark-whole-buffer)
;; (global-set-key "\C-i" 'isearch-forward)
;; (define-key isearch-mode-map (kbd "C-S-i") 'isearch-repeat-backward)
;; (define-key isearch-mode-map (kbd "C-i") 'isearch-repeat-forward)
;; (global-set-key (kbd "C-s") 'save-buffer)

;; === meow ===

;; I really like Meow but I'm keeping this disabled. I wish the cursor
;; were a single character selection like in Kakoune. Needing to start
;; a selection with `w` before extending does not spark joy.

;; (defun meow-setup ()
;;   (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;;   (meow-leader-define-key
;;    '("," . xref-pop-marker-stack)
;;    '("." . xref-find-definitions)
;;    '("f" . project-find-file)
;;    '("w" . other-window)
;;    '("W" . window-swap-states)
;;    '("o" . delete-other-windows)
;;    '("s" . split-window-right)
;;    '("v" . split-window-below)
;;    '("&" . +change-theme)
;;    '(";" . comment-line)
;;    '("K" . kill-this-buffer)
;;    '("d" . dired)
;;    '("b" . switch-to-buffer)
;;    '("r" . rg-project)
;;    '("f" . find-file)
;;    '("i" . imenu)
;;    '("a" . execute-extended-command)
;;    '("=" . org-store-link)
;;    '("p" . project-find-file)
;;    '("j" . project-switch-to-buffer)
;;    '("t" . tab-bar-switch-to-tab)
;;    '("l" . project-switch-project)
;;    '("y" . magit)
;;    '("l" . recentf-open-files)
;;    '("n" . org-roam-keymap)
;;    '(":" . eval-expression)
;;    '("e" . "C-x C-e")
;;    '("u" . undo-tree-visualize)
;;    ;; Use SPC (0-9) for digit arguments.
;;    '("1" . meow-digit-argument)
;;    '("2" . meow-digit-argument)
;;    '("3" . meow-digit-argument)
;;    '("4" . meow-digit-argument)
;;    '("5" . meow-digit-argument)
;;    '("6" . meow-digit-argument)
;;    '("7" . meow-digit-argument)
;;    '("8" . meow-digit-argument)
;;    '("9" . meow-digit-argument)
;;    '("0" . meow-digit-argument)
;;    '("/" . meow-keypad-describe-key)
;;    '("?" . meow-cheatsheet)
;;    '("@" . mark-sexp))
;;   (meow-normal-define-key
;;    '("0" . meow-expand-0)
;;    '("9" . meow-expand-9)
;;    '("8" . meow-expand-8)
;;    '("7" . meow-expand-7)
;;    '("6" . meow-expand-6)
;;    '("5" . meow-expand-5)
;;    '("4" . meow-expand-4)
;;    '("3" . meow-expand-3)
;;    '("2" . meow-expand-2)
;;    '("1" . meow-expand-1)
;;    '("-" . negative-argument)
;;    '(";" . meow-reverse)
;;    '("," . meow-inner-of-thing)
;;    '("." . meow-bounds-of-thing)
;;    '("<" . meow-beginning-of-thing)
;;    '(">" . meow-end-of-thing)
;;    '("a" . meow-append)
;;    '("A" . meow-open-below)
;;    '("b" . meow-back-word)
;;    '("B" . meow-back-symbol)
;;    '("c" . meow-change)
;;    '("d" . meow-delete)
;;    '("D" . meow-backward-delete)
;;    '("e" . meow-next-word)
;;    '("E" . meow-next-symbol)
;;    '("f" . meow-find)
;;    '("g" . meow-cancel-selection)
;;    '("G" . meow-grab)
;;    '("h" . meow-left)
;;    '("H" . meow-left-expand)
;;    '("i" . meow-insert)
;;    '("I" . meow-open-above)
;;    '("n" . meow-next)
;;    '("N" . meow-next-expand)
;;    '("p" . meow-prev)
;;    '("P" . meow-prev-expand)
;;    '("l" . meow-right)
;;    '("L" . meow-right-expand)
;;    '("m" . meow-join)
;;    '("s" . meow-search)
;;    '("o" . meow-block)
;;    '("O" . meow-to-block)
;;    '("j" . meow-yank)
;;    '("q" . meow-quit)
;;    '("Q" . meow-goto-line)
;;    '("r" . meow-replace)
;;    '("R" . meow-swap-grab)
;;    '("k" . meow-kill)
;;    '("t" . meow-till)
;;    '("u" . meow-undo)
;;    '("U" . meow-undo-in-selection)
;;    '("v" . meow-visit)
;;    '("w" . meow-mark-word)
;;    '("W" . meow-mark-symbol)
;;    '("x" . meow-line)
;;    '("X" . meow-goto-line)
;;    '("y" . meow-save)
;;    '("Y" . meow-sync-grab)
;;    '("z" . meow-pop-selection)
;;    '("'" . repeat)
;;    '("&" . meow-query-replace-regexp)
;;    '("%" . meow-query-replace)
;;    '("]" . scroll-up-command)
;;    '("[" . scroll-down-command)
;;    '("{" . backward-paragraph)
;;    '("}" . forward-paragraph)
;;    '("<escape>" . ignore)))

;; (use-package meow
;;   :ensure t
;;   :config
;;   (meow-setup)
;;   (meow-global-mode 1)
;;   (setq meow-use-clipboard t)
;;   (setq meow-visit-sanitize-completion nil)
;;   )

;; === lsp-bridge ===

;; This is a very ambitious package, and I like a lot of what it does,
;; but the external requirements are annoying. There's not really a good
;; way to get it set up on some of my systems, which restrict pip.
;; Requiring python is just annoying.

;; dependencies
;; (use-package markdown-mode
;;   :ensure t)

;; (use-package posframe
;;   :ensure t)

;; (use-package yasnippet
;;   :ensure t
;;   :config
;;   (yas-global-mode 1))


;; (use-package lsp-bridge
;;   :commands lsp-bridge-mode
;;   :load-path "~/.emacs.d/lisp/lsp-bridge"
;;   :ensure nil
;;   :bind
;;   (:map lsp-bridge-mode-map
;;         ("M-." . lsp-bridge-find-def)
;;         ("M-n i" . lsp-bridge-find-impl)
;;         ("M-n RET" . lsp-bridge-code-action)
;;         ("M-n ." . lsp-bridge-find-def-other-window)
;;         ("M-," . lsp-bridge-find-def-return)
;;         ("M-n d" . lsp-bridge-lookup-documentation)
;;         ("M-n r" . lsp-bridge-rename)
;;         ("M-n n" . lsp-bridge-diagnostic-jump-next)
;;         ("M-n p" . lsp-bridge-diagnostic-jump-prev)
;;         ("M-n l" . lsp-bridge-diagnostic-list)
;;         ("M-n q" . lsp-bridge-restart-process))

;;   :init
;;   (use-package markdown-mode)
;;   (use-package posframe)

;;   :hook
;;   (prog-mode . lsp-bridge-mode)

;;   :config
;;   (local-unset-key (kbd "M-,"))
;;   (local-unset-key (kbd "M-."))
;;   ;; (setq lsp-bridge-enable-auto-format-code nil)
;;   ;; (setq lsp-bridge-auto-format-code-idle 3)
;;   (setq markdown-enable-highlighting-syntax t)

;;   (require 'cl-lib)
;;   ;; (use-package format-all
;;   ;;   :config
;;   ;;   (add-hook 'prog-mode-hook 'format-all-mode)
;;   ;;   :bind
;;   ;;   (:map lsp-bridge-mode-map ;; no format-all-mode-map, use lsp bridge
;;   ;;         ("M-n f" . format-all-buffer)))
;;   )
;; (setq lsp-bridge-enable-log t)
;; (setq lsp-bridge-enable-debug t)

;; customizations

;; (use-package clipetty
;;     :ensure t
;;     :config
;;     (global-clipetty-mode))



;; (use-package smex
;;   :ensure t)

;; (use-package ido-completing-read+
;;   :ensure t)

;; (ido-mode 1)
;; (ido-everywhere 1)
;; (ido-ubiquitous-mode 1)

;; (global-set-key (kbd "M-x") 'smex)
;; (global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

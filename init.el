;; === Fonts ===

;; (set-face-attribute 'default nil :font "Fira Mono" :height 90)
;; (set-face-attribute 'default nil :font "monospace" :height 120)
;; (set-face-attribute 'default nil :font "Fira Mono" :height 120)
;; (set-face-attribute 'default nil :font "Fira Mono" :height 140)
;; lower weight fira moono
;; (set-face-attribute 'default nil :font "monospace" :height 100)
;; (set-face-attribute 'default nil :font "Monaco" :height 90)
;; (set-face-attribute 'default nil :font "Iosevka" :height 90)
;; (set-face-attribute 'default nil :font "Menlo" :height 140)
(set-face-attribute 'default nil :font "Menlo" :height 120)
;; (set-face-attribute 'default nil :font "Go Mono" :height 120)
;; (set-face-attribute 'default nil :font "Fira Mono" :height 140)

;; === Packages and Initialization ===

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'recentf)
(require 'uniquify)
(require 'view)

(use-package exec-path-from-shell
  :ensure t)
(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))
;; (add-to-list 'exec-path "/Users/jon/.local/bin")

;; Go mode setup
(use-package go-mode
  :ensure t
  :hook ((before-save . gofmt-before-save)
         (before-save . goimports-before-save)))

(defun goimports-before-save ()
  "Replace `gofmt' with `goimports' if it is installed."
  (if (executable-find "goimports")
      (let ((gofmt-command "goimports"))
        (gofmt-before-save))))

(add-hook 'go-mode-hook (lambda ()
                          (setq gofmt-command "goimports")
                          (add-hook 'before-save-hook 'gofmt-before-save nil t)))


(use-package undo-tree
  :diminish ;; Don't show an icon in the modeline
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

(use-package ansi-color
    :hook (compilation-filter . ansi-color-compilation-filter)) 

(defalias 'yes-or-no-p 'y-or-n-p)

(autoload 'zap-up-to-char "misc"
  "Kill up to, but not including ARGth occurrence of CHAR." t)

;; === Various Settings ===

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
      gptel-api-key ""
      gptel-model "gpt-4o")

(unless backup-directory-alist
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups")))))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; === Modes ===
(defun indent-last-pasted-region ()
  "Indent the last pasted region."
  (interactive)
  (when (and (boundp 'last-command)
             (or (eq last-command 'yank)
                 (eq last-command 'yank-pop)))
    (let ((mark-even-if-inactive t))
      (indent-region (region-beginning) (region-end)))))

;; bind it to Shift+tab
(global-set-key (kbd "<backtab>") 'indent-last-pasted-region)

(delete-selection-mode 1)
;; (xclip-mode 1)
;;(xterm-mouse-mode 1)
(recentf-mode 1)
(global-auto-revert-mode 1)
(show-paren-mode -1)
(savehist-mode 1)
(save-place-mode 1)
(show-paren-mode 1)
(ido-mode 'buffers)
(global-hl-line-mode -1)
;; (blink-cursor-mode 1)
(scroll-bar-mode -1)
;; (context-menu-mode 1) ;; crashing emacs on mac
(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)
;; (etags-regen-mode 1)

;; - turning off because tired of it jumping aroun
;; - turning on because it makes lookin at stuff with citre easier
;; (fido-vertical-mode 1)

(use-package smex
  :ensure t
  :init
  (smex-initialize))

;; ido + smex
;; (ido-mode t)
;; (setq ido-enable-flex-matching t)


(global-set-key (kbd "M-x") 'smex)
(global-set-key (kbd "M-X") 'smex-major-mode-commands)
;; ;; This is your old M-x.
(global-set-key (kbd "C-c C-c M-x") 'execute-extended-command)

(use-package cider :ensure t
    :bind ("C-c C-e" . cider-eval-print-last-expression))
(setq cider-repl-pop-to-buffer-on-connect nil)

(use-package zoom
  :ensure t)
(custom-set-variables
 '(zoom-size '(0.618 . 0.618)))

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
(define-key Info-mode-map [remap scroll-up-command] 'View-scroll-half-page-forward)
(define-key Info-mode-map [remap scroll-down-command] 'View-scroll-half-page-backward)

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
	 ("C-<" . 'mc/mark-previous-like-this))))

;; (use-package company
;;   :ensure t
;;   :init
;;   (global-company-mode))

;; (setq company-backends '(company-files company-dabbrev company-capf))

(when (file-directory-p "~/.emacs.d/lisp")
  (add-to-list 'load-path "~/.emacs.d/lisp")

  (when (file-directory-p "~/.emacs.d/lisp/witness")
    (add-to-list 'load-path "~/.emacs.d/lisp/witness")))

;; check to see if we have jai-mode
;; (require 'jai-mode)
;; (add-to-list 'auto-mode-alist '("\\.jai\\'" . jai-mode))

(use-package citre
  :defer t
  :init
  ;; This is needed in `:init' block for lazy load to work.
  (require 'citre-config)
  ;; Bind your frequently used commands.  Alternatively, you can define them
  ;; in `citre-mode-map' so you can only use them when `citre-mode' is enabled.
  (global-set-key (kbd "C-x c j") 'citre-jump)
  (global-set-key (kbd "C-x c J") 'citre-jump-back)
  (global-set-key (kbd "C-x c p") 'citre-ace-peek)
  (global-set-key (kbd "C-x c u") 'citre-relocate-tags-file)
  (global-set-key (kbd "C-x c u") 'citre-update-this-tags-file)
  :config
  (setq
   ;; Set these if readtags/ctags is not in your PATH.
   citre-readtags-program "/opt/homebrew/bin/readtags"
   citre-ctags-program "/opt/homebrew/bin/ctags"))
(add-hook 'jai-mode-hook 'citre-mode)
(add-hook 'c-mode-hook 'citre-mode)
(add-hook 'c++-mode-hook 'citre-mode)
(add-hook 'lua-mode-hook 'citre-mode)

(defun my-project-compile ()
  "Run compile using `my-current-project` as the base directory if set, otherwise in the project root."
  (interactive)
  (let ((default-directory (if (and (boundp 'my-current-project)
                                    my-current-project
                                    (not (string-empty-p my-current-project)))
                               my-current-project
                             (project-root (project-current t))))
        (command (or (and (boundp 'compile-command) compile-command)
                     "./build.sh")))
    (compile command)))

(defun my-cmake-compile ()
  "run make out of the build subdirectory of the project root"
  (interactive)
    (let ((default-directory (concat (project-root (project-current t)) "/build"))
            (command "ninja"))
      (compile command)))

(defun my-fips-compile ()
  "run make out of the build subdirectory of the project root"
  (interactive)
    (let ((default-directory  (project-root (project-current t)))
            (command "./fips build"))
      (compile command)))
(defun my-fips-compile ()
  "Run make out of the build subdirectory of the project root."
  (interactive)
  (let ((default-directory (project-root (project-current t)))
        (command "./fips build"))
    (compile command)
    (with-current-buffer "*compilation*"
      (font-lock-mode -1))))

(defun disable-font-lock-in-compilation-buffer ()
  "Disable syntax highlighting in the compilation buffer."
  (font-lock-mode -1))

(add-hook 'compilation-mode-hook 'disable-font-lock-in-compilation-buffer)

;; use oberon-mode for .MOD files
(add-to-list 'auto-mode-alist '("\\.MOD\\'" . oberon-mode))

;; open iterm in cwd
(defun open-iterm-in-cwd ()
  (interactive)
    (shell-command (concat "open -a iTerm " default-directory) nil nil))

(defun open-terminal-in-cwd ()
  (interactive)
  (shell-command (concat "open -a Terminal " default-directory) nil nil))

(defun open-terminal-in-project-root ()
    (interactive)
    (shell-command (concat "open -a Terminal " (project-root (project-current t))) nil nil))

(defun my-compile ()
  "Run compile without prompt."
  (interactive)
  ;; Use the existing compile-command, or "make" if compile-command is empty
  (let ((command (if (and compile-command (not (string-empty-p compile-command)))
                     compile-command
                   "make")))
    (compile command)))

(defun jai-compile-file ()
  "Compile the current Jai buffer."
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
         (base-name (file-name-sans-extension file-name))
         (compile-command (format "jai-macos %s.jai" base-name)))
    (compile compile-command)))

(defun jai-run-file ()
  "Run the build file for the current Jai buffer."
  (interactive)
  (let* ((file-name (file-name-nondirectory (buffer-file-name)))
         (base-name (file-name-sans-extension file-name))
         (run-command (format "./%s" base-name)))
    (compile run-command)))

(defun python-compile-file ()
  "Save the current buffer and run the Python file asynchronously in a dedicated, cleared buffer."
  (interactive)
  (save-buffer)  ; Save the current buffer
  (let* ((file-name (buffer-file-name))
         (default-directory (file-name-directory file-name))
         (command (format "python3 %s" (shell-quote-argument file-name)))
         (buffer-name "*Python Output*"))
    (when (get-buffer buffer-name)
      (with-current-buffer buffer-name
        (erase-buffer)))
    (async-shell-command command buffer-name)))

(global-set-key (kbd "<f6>") 'jai-run-file)
(global-set-key (kbd "<f7>") 'copilot-mode)

(defun lua-love-compile ()
  "Compile the current Lua buffer for LÖVE projects."
  (interactive)
  (let* ((project-dir (expand-file-name (project-root (project-current t)))) ; Expand the path
         (compile-command (format "cd %s && love ." (shell-quote-argument project-dir))))
    (compile compile-command)))


(defun dynamic-compile ()
  "Dynamically dispatch compile command based on current major mode."
  (interactive)
  (cond
   ((eq major-mode 'jai-mode) (jai-compile-file))
   ((eq major-mode 'python-mode) (python-compile-file))
   ((eq major-mode 'lua-mode) (lua-love-compile))
   (t (my-compile))))

(defun my-run ()
  "A generalized run command. Prompts the user for a command to run and runs it asynchronously."
  (interactive)
  (let ((command (read-shell-command "Run command: ")))
    (async-shell-command command "*Async Shell Command*")))

(defun throwaway-run ()
  (interactive)
  (async-shell-command "bash -c 'cd /Users/jon/development/cpp/sdl3-cmake/ && /Users/jon/development/cpp/sdl3-cmake/build/sdl-min.app/Contents/MacOS/sdl-min'"))


(defun dynamic-run ()
  "Dynamically dispatch compile command based on current major mode."
  (interactive)
  (cond
   ((eq major-mode 'jai-mode) (jai-run-file))
   ((eq major-mode 'c++-mode) (jai-run-file))
   (t (my-run))))

(global-set-key (kbd "<f5>") 'my-project-compile)
(global-set-key (kbd "<f5>") 'my-cmake-compile)
(global-set-key (kbd "<f5>") 'my-fips-compile)
(global-set-key (kbd "<f5>") 'code-cells-eval)
(global-set-key (kbd "<f5>") 'dynamic-compile)

(global-set-key (kbd "<f6>") 'dynamic-run)

;; (use-package orderless
;;   :ensure t
;;   :init
;;   (setq completion-styles '(orderless flex)
;;         completion-category-defaults nil
;;         completion-category-overrides '((file (styles partial-completion)))))

  ;; Enable rich annotations using the Marginalia package
;; (use-package marginalia
;;   :ensure t
;;   ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
;;   ;; available in the *Completions* buffer, add it to the
;;   ;; `completion-list-mode-map'.
;;   :bind (:map minibuffer-local-map
;;          ("M-A" . marginalia-cycle))
;;   :init
;;   (marginalia-mode))

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package rustic
  :disabled t)

(use-package expand-region
  :ensure t
  :bind ("C-=" . 'er/expand-region))

(defun my/copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (dabbrev-expand)))

(defun my/copilot-c-g ()
  (interactive)
  (or (copilot-clear-overlay)
      (keyboard-quit)))

;; (use-package yasnippet
;;   :ensure t
;;   :config
;;   (yas-global-mode 1))

(with-eval-after-load 'copilot
  (define-key copilot-mode-map (kbd "C-<tab>") #'copilot-accept-completion)
  (define-key copilot-mode-map (kbd "C-g") #'my/copilot-c-g)
  (define-key copilot-mode-map (kbd "C-c <tab>") #'copilot-accept-completion))

(when (file-readable-p "~/.emacs.d/custom.el")
  (load "~/.emacs.d/custom.el"))

;; (add-to-list 'load-path "/Users/jon/.emacs.d/copilot.el")
;; (require 'copilot)

(with-eval-after-load 'multiple-cursors
  (global-set-key (kbd "C-S-<mouse-1>") 'mc/add-cursor-on-click))

(defun dired-do-llm-format (&optional arg)
  "Create a buffer with the contents of the marked (or next ARG) files, formatted in Markdown."
  (interactive)
  (let ((file-list (dired-get-marked-files t arg nil nil t))
        (buffer (generate-new-buffer "*LLM Format*")))
    (with-current-buffer buffer
      (markdown-mode)
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

(defun dired-do-llm-format (&optional arg)
  "Create a buffer with the contents of the marked (or next ARG) files, formatted in Markdown."
  (interactive)
  (let ((file-list (dired-get-marked-files t arg nil nil t))
        (buffer (generate-new-buffer "*LLM Format*"))
        (header-text (read-string "Enter text to insert at the top of the buffer: ")))
    (with-current-buffer buffer
      (markdown-mode)
      ;; Insert the header text at the top
      (insert header-text "\n\n")
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

(defun ibuffer-do-llm-format (&optional arg)
  "Create a buffer with the contents of the marked (or next ARG) buffers, formatted in Markdown."
  (interactive "P")
  (let ((buffer-list (ibuffer-get-marked-buffers))
        (new-buffer (generate-new-buffer "*LLM Format*"))
        (header-text (read-string "Enter text to insert at the top of the buffer: ")))
    (with-current-buffer new-buffer
      (markdown-mode)
      ;; Insert the header text at the top
      (insert header-text "\n\n")
      (dolist (buf buffer-list)
        ;; Insert the buffer name with backticks
        (insert "```" (buffer-name buf) "\n")
        ;; Save the position
        (let ((start (point)))
          ;; Insert the buffer contents
          (insert-buffer-substring buf)
          ;; Insert the closing backticks
          (goto-char (point-max))
          (insert "\n```\n")))
      (goto-char (point-min)))
    (switch-to-buffer new-buffer)))

(with-eval-after-load 'ibuffer
  (define-key ibuffer-mode-map (kbd "b") 'ibuffer-do-llm-format))

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

(defun goto-bell ()
  "uses sshx to navigate to solaire@bell:/home/solaire"
  (interactive)
  (dired "/sshx:solaire@bell:/home/solaire"))

(defun goto-windows ()
  "Uses sftp to navigate to the C:\\Users\\toast directory on a Windows machine."
  (interactive)
  (dired "/sftp:toast@192.168.1.47:/C:/Users/toast"))

(defun load-oculus-tags ()
  "uses sshx to navigate to solaire@bell:/home/solaire/development/cpp/oculus"
  (interactive)
  (visit-tags-table "/sshx:solaire@bell:/home/solaire/development/cpp/oculus/TAGS"))

;; in rustic-compilation-mode, use `p` for previous-error-no-select
(with-eval-after-load 'rustic
  (define-key rustic-compilation-mode-map (kbd "p") 'previous-error-no-select))

;; mitsuharu

(defun je/reconfigure-nsappearance ()
  (let ((appearance (plist-get (mac-application-state) :appearance)))
    ;; Disable all currently enabled themes.
    (mapc #'disable-theme custom-enabled-themes)
    ;; Now load the theme based on the macOS appearance setting.
    (if (string-equal appearance "NSAppearanceNameDarkAqua")
        (load-theme 'standard-dark t)
      (load-theme 'standard-light t))))

(add-hook 'mac-effective-appearance-change-hook 'je/reconfigure-nsappearance)

;; (use-package pixel-scroll
;;   :bind
;;   ([remap scroll-up-command]   . pixel-scroll-interpolate-down)
;;   ([remap scroll-down-command] . pixel-scroll-interpolate-up)
;;   :custom
;;   (pixel-scroll-precision-interpolate-page t)
;;   :init
;;   (pixel-scroll-precision-mode 1))

(use-package paredit
  :ensure t
  :hook ((clojure-mode
          scheme-mode
          emacs-lisp-mode
          lisp-mode
          racket-mode) . enable-paredit-mode)
  :config
  (show-paren-mode t))

(use-package ox-hugo
  :ensure t)
  
(defun format-comment (comment)
  "Format COMMENT to ensure each line is at most 80 characters long without breaking words."
  (let ((words (split-string comment))
        (max-width 80)
        (line "")
        (formatted-comment ""))
    (dolist (word words)
      (if (> (+ (length line) (length word) 1) max-width)
          (setq formatted-comment (concat formatted-comment line "\n")
                line ""))
      (setq line (if (string= line "")
                     word
                   (concat line " " word))))
    (concat formatted-comment line)))

(defun format-comment-at-point ()
  "Format the comment line at the point to be no more than 80 characters long without breaking words."
  (interactive)
  (let* ((max-width 78)  ; Adjusted for the space after the comment character
         (line-start (line-beginning-position))
         (line-end (line-end-position))
         (full-line (buffer-substring line-start line-end))
         (comment-prefix (if comment-start
                             (concat comment-start " ")
                           "# "))  ; Default to "#" if comment-start is nil
         (words (cdr (split-string full-line)))  ; Skip the first element assuming it's the comment character
         (formatted-comment comment-prefix)
         (line ""))
    (dolist (word words)
      (if (> (+ (length line) (length word) 1) max-width)
          (progn
            (setq formatted-comment (concat formatted-comment line "\n" comment-prefix))
            (setq line "")))
      (setq line (concat line (if (string= line "") "" " ") word)))
    (setq formatted-comment (concat formatted-comment line))

    (save-excursion
      (delete-region line-start line-end)
      (goto-char line-start)
      (insert formatted-comment))))

(global-set-key (kbd "C-c C-l") 'format-comment-at-point)

;; (setq frame-background-mode 'dark)

;; defun to set cursor color to dark grey
(defun greyme ()
  (interactive)
  (set-cursor-color "dark grey"))

(defun greenme ()
  (interactive)
  (set-cursor-color "chartreuse2"))

(defun my-c++-mode-setup ()
  (c-set-offset 'innamespace 0))

(add-hook 'c++-mode-hook 'my-c++-mode-setup)

(defun toggle-standard-themes ()
  (interactive)
  (if (eq (car custom-enabled-themes) 'standard-dark)
      (progn
        (disable-theme 'standard-dark)
        (load-theme 'standard-light t))
    (progn
      (disable-theme 'standard-light)
      (load-theme 'standard-dark t))))

(when (>= emacs-major-version 28)
  ;; emacs 28 or later. use icomplete-vertical-mode and set flex match
  (setq completion-styles '(flex))
  (icomplete-vertical-mode 1))

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


(with-eval-after-load 'code-cells
  (let ((map code-cells-mode-map))
    (define-key map "n" (code-cells-speed-key 'code-cells-forward-cell))
    (define-key map "p" (code-cells-speed-key 'code-cells-backward-cell))
    (define-key map "e" (code-cells-speed-key 'code-cells-eval))
    (define-key map (kbd "TAB") (code-cells-speed-key 'outline-cycle))))

;; command to wrap selection in <center> tag
(defun wrap-in-center-tag ()
  "Wrap the selected text in a <center> tag."
  (interactive)
  (let ((start (region-beginning))
        (end (region-end)))
    (goto-char end)
    (insert "</center>")
    (goto-char start)
    (insert "<center>")))
;; bind it to C-c C-e in markdown
(add-hook 'markdown-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-e") 'wrap-in-center-tag)))
(defun disable-eldoc-in-python-mode ()
  "Disable eldoc-mode in python-mode."
  (when (derived-mode-p 'python-mode)
    (eldoc-mode -1)))

(use-package auto-virtualenv
  :ensure t
  :init
  (use-package pyvenv
    :ensure t)
  :config
  (add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)
  ;; (add-hook 'projectile-after-switch-project-hook 'auto-virtualenv-set-virtualenv)  ;; If using projectile
  )
(add-hook 'python-mode-hook 'auto-virtualenv-set-virtualenv)

(use-package perspective
  :bind
  ("C-x C-b" . persp-list-buffers) ; or use a nicer switcher, see below
  :custom
  (persp-mode-prefix-key (kbd "C-c M-p")) ; pick your own prefix key here
  :init
  (persp-mode))

(when (file-readable-p "~/.emacs.d/persp-state")
  (persp-state-load "~/.emacs.d/persp-state"))

(add-hook 'window-configuration-change-hook 'auto-virtualenv-set-virtualenv)
(add-hook 'focus-in-hook 'auto-virtualenv-set-virtualenv)

(add-hook 'python-mode-hook 'disable-eldoc-in-python-mode)
(add-hook 'python-mode-hook 'code-cells-mode-maybe)


;; === Packages and Initialization ===

(set-face-attribute 'default nil :font "Berkeley Mono" :height 140)

(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(require 'recentf)
(require 'uniquify)
(require 'view)

;; (use-package xclip
;;   :ensure t)

(use-package clipetty
  :ensure t)

;; if this is a terminal session, enable global clipetty mode
(if (not (display-graphic-p))
    (global-clipetty-mode 1))

(use-package exec-path-from-shell
  :ensure t)

(when (memq window-system '(mac ns x))
  (exec-path-from-shell-initialize))

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
      ido-enable-flex-matching t
      completion-cycle-threshold 10
      modus-themes-fringes nil
      save-interprogram-paste-before-kill t
      compilation-scroll-output t
      ;; make cursor a line in all buffers
      )

;; (set-default 'cursor-type 'bar)

(unless backup-directory-alist
  (setq backup-directory-alist `(("." . ,(concat user-emacs-directory
                                                 "backups")))))
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)

;; (load-file "/Users/jon/.emacs.d/meow-setup.el")

;; === Modes ===

(delete-selection-mode 1)
;; (xclip-mode 1)
(xterm-mouse-mode 1)
(recentf-mode 1)
(global-auto-revert-mode 1)
(show-paren-mode 1)
(savehist-mode 1)
(save-place-mode 1)
;; (ido-mode 1)
;; (global-hl-line-mode 1)
;; (blink-cursor-mode 1)
(scroll-bar-mode -1)
;; (context-menu-mode 1) ;; crashing emacs on mac
(menu-bar-mode -1)
(tool-bar-mode -1)
(column-number-mode 1)
(etags-regen-mode 1)

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
	 ("C-<" . 'mc/mark-previous-like-this))))


;; (use-package company
;;   :ensure t
;;   :init
;;   (global-company-mode))

(when (file-directory-p "~/.emacs.d/lisp")
  (add-to-list 'load-path "~/.emacs.d/lisp")

  (when (file-directory-p "~/.emacs.d/lisp/witness")
    (add-to-list 'load-path "~/.emacs.d/lisp/witness")))

(require 'jai-mode)
(add-to-list 'auto-mode-alist '("\\.jai\\'" . jai-mode))

;; (require 'simpc-mode)
;; (add-to-list 'auto-mode-alist '("\\.[hc]\\(pp\\)?\\'" . simpc-mode))
;; (add-hook 'simpc-mode-hook 'ggtags-mode)

;; (add-hook 'c++-mode-hook 'ggtags-mode)

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
(defun my-project-compile ()
  "Run compile using `my-current-project` as the base directory if set, otherwise in the project root."
  (interactive)
  (let ((default-directory (if (and (boundp 'my-current-project)
                                    my-current-project
                                    (not (string-empty-p my-current-project)))
                               my-current-project
                             (project-root (project-current t))))
        (command (or (and (boundp 'compile-command) compile-command)
                     "make")))
    (compile command)))

;; use oberon-mode for .MOD files
(add-to-list 'auto-mode-alist '("\\.MOD\\'" . oberon-mode))

;; -(defun open-iterm-in-vc-root ()
;; -  (interactive)
;; -  (let ((project-root (caddr (project-current))))
;; -    (shell-command (concat "open -a iTerm " project-root) nil nil)))

;; open iterm in cwd
(defun open-iterm-in-cwd ()
  (interactive)
    (shell-command (concat "open -a iTerm " default-directory) nil nil))

;; TODO: recognize if this is has a makefile in the root directory
;; if I need CMake I'll just use someone else's package
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
         (compile-command (format "jai-macos %s.jai && ./%s" base-name base-name)))
    (compile compile-command)))

(defun python-compile-file ()
    "Compile the current Python buffer."
    (interactive)
    (let* ((file-name (file-name-nondirectory (buffer-file-name)))
             (base-name (file-name-sans-extension file-name))
             (compile-command (format "python3 %s.py" base-name)))
        (compile compile-command)))

(global-set-key (kbd "<f7>") 'clang-format-buffer)

(defun dynamic-compile ()
  "Dynamically dispatch compile command based on current major mode."
  (interactive)
  (cond
   ((eq major-mode 'jai-mode) (jai-compile-file))
   ((eq major-mode 'python-mode) (python-compile-file))
   (t (my-compile))))

(global-set-key (kbd "<f5>") 'dynamic-compile)


(use-package orderless
  :ensure t
  :init
  (setq completion-styles '(orderless flex)
        completion-category-defaults nil
        completion-category-overrides '((file (styles partial-completion)))))

  Enable rich annotations using the Marginalia package
(use-package marginalia
  :ensure t
  ;; Bind `marginalia-cycle' locally in the minibuffer.  To make the binding
  ;; available in the *Completions* buffer, add it to the
  ;; `completion-list-mode-map'.
  :bind (:map minibuffer-local-map
         ("M-A" . marginalia-cycle))

  ;; The :init section is always executed.
  :init

  ;; Marginalia must be activated in the :init section of use-package such that
  ;; the mode gets enabled right away. Note that this forces loading the
  ;; package.
  (marginalia-mode))


;; (use-package vertico
;;   :ensure t
;;   :init
;;   (vertico-mode)
;;   (setq vertico-scroll-margin 0)
;;   (setq vertico-count 20)
;;   ;; (setq vertico-resize t)
;;   (setq vertico-cycle t))

(use-package which-key
  :ensure t
  :init
  (which-key-mode))

(use-package rustic
  :disabled t)

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

(add-to-list 'load-path "/Users/jon/.emacs.d/lisp/copilot.el")
(require 'copilot)

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

;; set variable pitch font to helvetica neue
;; (set-face-attribute 'variable-pitch nil :font "Helvetica Neue" :height 140)

;; mitsuharu

(defun je/reconfigure-nsappearance ()
  (let ((appearance (plist-get (mac-application-state) :appearance)))
    ;; Disable all currently enabled themes.
    (mapc #'disable-theme custom-enabled-themes)
    ;; Now load the theme based on the macOS appearance setting.
    (if (string-equal appearance "NSAppearanceNameDarkAqua")
        (load-theme 'modus-vivendi t)
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
  :config
  (autoload 'enable-paredit-mode "paredit" t)
  (add-hook 'emacs-lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
  (add-hook 'lisp-mode-hook #'enable-paredit-mode)
  (add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
  (add-hook 'scheme-mode-hook #'enable-paredit-mode)
  (add-hook 'clojure-mode-hook #'enable-paredit-mode)
  (add-hook 'racket-mode-hook #'enable-paredit-mode)
  )
  
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

;; (autoload 'gerbil-mode "gerbil-mode" "Gerbil editing mode." t)
;; (defvar gerbil-program-name
;;   (expand-file-name  "/opt/homebrew/bin/gxi"))

;;(setq scheme-program-name gerbil-program-name)


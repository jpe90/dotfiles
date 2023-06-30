(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)
;;(setq frame-background-mode dark)

(setq package-deps '( better-defaults
                      magit
                      cider
                      inf-clojure
                      clojure-mode
                      slime
                      paredit
                       ))

;;(load (expand-file-name "~/.roswell/helper.el"))

(dolist (package package-deps)
  (unless (package-installed-p package)
    (package-refresh-contents)
    (package-install package)))


(when (string= system-type "darwin")
  (setq dired-use-ls-dired nil))

;; faster startup
(setq gc-cons-threshold 64000000)
(add-hook 'after-init-hook #'(lambda ()
                               (setq gc-cons-threshold 800000)))

(require 'recentf)
(require 'better-defaults)

(org-babel-do-load-languages
 'org-babel-load-languages
 '((emacs-lisp . t) ;; Other languages
   (shell . t)
   (python . t) ))

(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))

;; assignment

(setq python-shell-interpreter "remote-python"
      python-shell-interpreter-args "-i --simple-prompt")
(setq
 mac-option-modifier 'meta
 mac-command-modifier 'control
 backup-directory-alist (list (cons ".*" backup-dir))
 auto-save-list-file-prefix autosave-dir
 auto-save-file-name-transforms `((".*" ,autosave-dir t))
 set-mark-command-repeat-pop t     ;; cycle thru marks w/ c-space
 mouse-wheel-progressive-speed nil ;; turn off mouse acceleration
 read-file-name-completion-ignore-case t ;;; case insenstive autocompletion
 dired-kill-when-opening-new-dired-buffer t ;; stop dired from cluttering buffer list
 c-default-style "k&r"
 create-lockfiles nil
 minibuffer-prompt-properties
 '(read-only t cursor-intangible t face minibuffer-prompt)
 ;; indent with spaces
 c-basic-offset 4
 dired-deletion-confirmer '(lambda (x) t)
 split-height-threshold nil ;; only open horizontal splits (works on searches)
 undo-limit 20000000
 undo-strong-limit 40000000
 inhibit-startup-screen t
 column-number-mode t
 org-startup-with-inline-images t
 org-startup-folded t
;;; gdb setup
 ;; use gdb-many-windows by default
 gdb-many-windows nil
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 esup-depth 0 ;; for profiling
 recentf-max-menu-items 500
 recentf-max-saved-items 500
 history-length 1000
 xref-search-program 'ripgrep
 mouse-autoselect-window t
 tags-revert-without-query t
 python-shell-completion-native-disabled-interpreters '("python3")
 edebug-print-length nil
 edebug-print-level nil
 catppuccin-flavor 'mocha ;; or 'frappe, 'latte, 'macchiato, or 'mocha
 magit-save-repository-buffers nil
 inferior-lisp-program "sbcl"
 debug-on-error t
 custom-file "~/.emacs.d/custom.el")
(load custom-file)

(setq-default indent-tabs-mode nil)

;; start fullscreen
(add-to-list 'default-frame-alist '(fullscreen . maximized))

;; enable modes

(global-so-long-mode 1)
(blink-cursor-mode 0)
(xterm-mouse-mode 1)
(if (display-graphic-p)
    (scroll-bar-mode -1))
(electric-pair-mode 1)
(recentf-mode 1)
(delete-selection-mode 1)

;; add hooks

(defun my-comint-shorten-long-lines (text)
  (let* ((regexp "\\(.\\{75\\}[;,: ]\\)")
         (shortened-text (replace-regexp-in-string regexp "\\1\n" text)))
    (if (string= shortened-text text)
        text
      shortened-text)))

(add-hook 'comint-preoutput-filter-functions 'my-comint-shorten-long-lines)
(add-hook 'html-mode-hook
          (lambda ()
            (setq c-basic-offset 2)))
(add-hook 'c-mode-hook
          (lambda ()
            (setq-local fill-column 80)
            (c-set-offset 'arglist-intro '+)
            (c-set-offset 'arglist-close 0)))
(add-hook 'write-file-functions 'delete-trailing-whitespace)
;; (add-hook 'org-mode-hook
;;           (lambda ()
;;             (local-set-key (kbd "C-c C-l") 'org-store-link)))

(add-hook 'flymake-mode-hook
          (lambda ()
            (local-set-key (kbd "S-<f2>") 'flymake-goto-prev-error)
            (local-set-key (kbd "<f2>") 'flymake-goto-next-error)))
(add-hook 'emacs-lisp-mode-hook
          (lambda ()
            (paredit-mode 1)
            (local-set-key (kbd "C-c C-c") 'eval-buffer)))
(add-hook 'fennel-mode-hook
          (lambda ()
            (paredit-mode 1)
            ))
;; (add-hook 'fennel-mode-hook 'fennel-proto-repl-minor-mode)
(add-hook 'c-mode-hook
          (lambda ()
            (local-set-key (kbd "C-c C-r") #'compile)))
(add-hook 'minibuffer-setup-hook #'cursor-intangible-mode)
(add-hook 'org-mode-hook
          (lambda ()
            (visual-line-mode 1)))
(add-hook 'markdown-mode-hook #'variable-pitch-mode)
(with-eval-after-load 'dired-mode
  (local-unset-key (kbd "o"))
  (local-unset-key (kbd "C-o")))
(add-hook 'janet-mode-hook (lambda ()
                             (inf-janet-minor-mode 1)
                             (paredit-mode 1)))
(add-hook 'evil-mode-hook (lambda ()
                            (linum-relative-global-mode 1)
                            (linum-mode 1)
                            (evil-surround-mode 1)
                            (evil-global-set-key 'insert (kbd "C-SPC") 'hippie-expand)))
(add-hook 'lisp-mode-hook (lambda ()
                            (paredit-mode 1)))

(add-hook 'eglot-mode-hook #'company-mode)
(with-eval-after-load 'paredit-mode
  (define-key paredit-mode-map (kbd "M-{") 'paredit-wrap-curly)
  (define-key paredit-mode-map (kbd "M-[") 'paredit-wrap-square))
(with-eval-after-load 'lua-mode
  (setq lua-indent-level 2)
  (setq lua-indent-nested-block-content-align nil)
  (setq lua-indent-close-paren-align nil)
  (setq lua-indent-string-contents t)

  (define-key lua-mode-map (kbd "C-c C-b") 'lua-send-buffer)
  (define-key lua-mode-map (kbd "C-c C-l") 'lua-send-current-line)
  (define-key lua-mode-map (kbd "C-c C-f") 'lua-send-defun)
  (define-key lua-mode-map (kbd "C-c C-r") 'lua-send-region)
  (define-key lua-mode-map (kbd "C-c C-z") 'lua-show-process-buffer))

;; ;; Escape C-x and C-c in terminal mode
;; (add-hook 'term-mode-hook
;;           (lambda ()
;;             ;; Hack to set two escape chars.
;;             (let (term-escape-char)
;;               (term-set-escape-char ?\C-x))
;;             (let (term-escape-char)
;;               (term-set-escape-char ?\C-c))))

(add-hook 'emacs-startup-hook #'display-startup-time)

;; defadvice

(defadvice yank (after indent-region activate)
  (when (member major-mode '(c++-mode emacs-lisp-mode python-mode c-mode go-mode java-mode clojure-mode haskell-mode scheme-mode))
    (unless mark-active
      (exchange-point-and-mark))
    (indent-region (region-beginning) (region-end) nil)
    (goto-char (region-end))))

;; functions

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

(defun open-iterm-in-vc-root ()
  (interactive)
  (let ((project-root (caddr (project-current))))
    (shell-command (concat "open -a iTerm " project-root) nil nil)))

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

(defun toggle-case (first-lower-p)
  "Toggle between camelcase and underscore notation for the
symbol at point. If prefix arg, C-u, is supplied, then make first
letter of camelcase lowercase."
  (interactive "P")
  (save-excursion
    (let* ((bounds (bounds-of-thing-at-point 'symbol))
           (start (car bounds))
           (end (cdr bounds))
           (currently-using-underscores-p (progn (goto-char start)
                                                 (re-search-forward "_" end t))))
      (if currently-using-underscores-p
          (progn
            (replace-string "_" " " nil start end)
            (upcase-initials-region start end)
            (replace-string " " "" nil start end)
            (when first-lower-p
              (downcase-region start (1+ start))))
        (replace-regexp "\\([A-Z]\\)" "_\\1" nil (1+ start) end)
        (downcase-region start (cdr (bounds-of-thing-at-point 'symbol)))))))

(defun revert-all-buffers ()
  "Refreshes all open buffers from their respective files"
  (interactive)
  (let* ((list (buffer-list))
         (buffer (car list)))
    (while buffer
      (when (and (buffer-file-name buffer)
                 (not (buffer-modified-p buffer)))
        (set-buffer buffer)
        (revert-buffer t t t))
      (setq list (cdr list))
      (setq buffer (car list))))
  (message "Refreshed open files"))

(defun highlight-marked-text-and-unhighlight ()
  "Highlight all instances of the marked text in the buffer, and then unhighlight when the user presses RET."
  (interactive)
  (let ((marked-text (buffer-substring-no-properties (region-beginning) (region-end))))
    (highlight-regexp marked-text)
    (message "Highlighted text. Press RET to unhighlight.")
    (read-event)
    (unhighlight-regexp marked-text)
    (message "Unhighlighted text.")))

(defun copy-current-kill-to-clipboard ()
  (interactive)
  (shell-command (concat "cat " (current-kill 0) " | pbcopy") ))

(defun display-startup-time ()
  (message
   "Emacs loaded in %s with %d garbage collections."
   (format
    "%.2f seconds"
    (float-time
     (time-subtract after-init-time before-init-time)))
   gcs-done))

;; key bindings

(define-key global-map (kbd "C-z") nil)
(define-key global-map (kbd "C-c C-l") #'org-store-link)
(define-key global-map (kbd "C-<next>") nil)
(define-key global-map "\eo" #'previous-buffer)
(define-key global-map (kbd "C-M-o") #'delete-other-windows)
(define-key global-map "" 'other-window)
(define-key global-map (kbd "C-;") #'comment-line)
(define-key global-map [f2] nil)
(define-key global-map (kbd "<next>") (lambda () (interactive) (View-scroll-half-page-forward)))
(define-key global-map (kbd "<prior>") (lambda () (interactive) (View-scroll-half-page-backward)))
(define-key global-map (kbd "C-,") 'project-find-regexp)
(define-key global-map (kbd "C-\\") 'project-switch-to-buffer)
(define-key global-map (kbd "C-x b") 'switch-to-buffer)
;; (define-key global-map "\e3" 'exchange-point-and-mark)
(define-key global-map "\e`" #'other-frame)
(define-key global-map (kbd "<C-left>") #'back-to-indentation)
(define-key global-map (kbd "<C-right>") #'move-end-of-line)
(define-key global-map (kbd "C-c C-f") 'format-all-buffer)
(define-key global-map (kbd "C-c r") 'project-compile)
(define-key global-map "\e1" 'mark-advance-line)
(define-key global-map "\e2" 'mark-defun)
;; (define-key global-map "\e3" 'project-dired)
(define-key global-map "\e3" 'bookmark-in-project-toggle)
(define-key global-map "\es" 'cua-rectangle-mark-mode)
;; (define-key global-map "\e/" 'project-find-regexp)
(define-key global-map "\e4" 'yank-filename-and-line)
(define-key global-map "\e5" 'visit-source)
(define-key global-map "\e6" 'bookmark-in-project-jump-next)
(define-key global-map "\e7" 'bookmark-in-project-jump-previous)
(define-key global-map "\en" 'forward-paragraph)
(define-key global-map "\ep" 'backward-paragraph)
(define-key global-map (kbd "C-.") 'project-find-regexp)
(define-key global-map "\C-x\ \C-r" 'recentf-open-files)
(define-key global-map [S-tab] 'indent-for-tab-command)
(define-key global-map (kbd "TAB") 'hippie-expand)
(define-key global-map (kbd "C-x t") 'beginning-of-buffer)
(define-key global-map (kbd "C-x e") 'end-of-buffer)
(define-key global-map (kbd "C-S-y") 'replace-yank)
(define-key global-map (kbd "\e%") 'query-replace-regexp)
(define-key global-map (kbd "<f2>") 'cua-rectangle-mark-mode)

;; hippie function expansion wrecks paren balancing
(dolist (f '(try-expand-line try-expand-list))
  (setq hippie-expand-try-functions-list
        (remq f hippie-expand-try-functions-list)))

(defun my/copilot-tab ()
  (interactive)
  (or (copilot-accept-completion)
      (dabbrev-expand)))

(defun my/copilot-c-g ()
  (interactive)
  (or (copilot-clear-overlay)
      (keyboard-quit)))

(with-eval-after-load 'copilot
  (define-key copilot-mode-map (kbd "C-<tab>") #'copilot-accept-completion)
  (define-key copilot-mode-map (kbd "C-g") #'my/copilot-c-g))

(define-key emacs-lisp-mode-map (kbd "<f5>") 'eval-buffer)

(add-to-list 'auto-mode-alist '("\\.mm\\'" . objc-mode))
(add-to-list 'auto-mode-alist '("\\.metal\\'" . c++-mode))
;; (add-to-list 'auto-mode-alist '("\\.go\\'" .
;;                                 (lambda ()
;;                                   (go-ts-mode))))
(add-to-list 'auto-mode-alist '("\\.swift\\'" . swift-mode))

(require 'package)
(add-to-list 'package-archives '("melpa" . "http://melpa.org/packages/"))

(require 'use-package)

(setq mac-option-modifier 'meta)

(defun enable-cua ()
  (cua-mode t)
  (global-set-key (kbd "C-<up>") #'scroll-down-command)
  (global-set-key (kbd "C-<down>") #'scroll-up-command)
  (global-set-key (kbd "C-<left>") #'beginning-of-line)
  (global-set-key (kbd "C-<right>") #'end-of-line))

(defun first-time-load ()
  (package-initialize)
(package-refresh-contents)
(package-install 'use-package))

(first-time-load)

(defun load-if-exists (file)
  (if (file-exists-p file)
      (load-file file)
    (message (concat file " doesn't exist"))))


(defvar my-customizations '("~/.emacs.d/lisp/platform.el"
                            ;; "~/.emacs.d/lisp/mariana/mariana-theme.el"
                            ;; "~/.emacs.d/lisp/uwu.el/uwu-theme.el"
                            ;; "~/.emacs.d/lisp/jetbrains-darcula-emacs-theme/jetbrains-darcula-theme.el"
                            ;; "~/.emacs.d/lisp/doom-alabaster-theme.el"
                            ))

;; (mapc #'load-if-exists my-customizations)

;; TODO:
;; - fn that highlights current line or, if mark is active, expands to next line

(defun my-reload-dir-locals-for-current-buffer ()
  "reload dir locals for the current buffer"
  (interactive)
  (let ((enable-local-variables :all))
    (hack-dir-local-variables-non-file-buffer)))

(defun make-transparent ()
  (set-frame-parameter (selected-frame) 'alpha '(85 85))
  (add-to-list 'default-frame-alist '(alpha 85 85)))

(defun insert-org-codeblock (language)
  (interactive "sEnter language for code block: ")
  (insert "#+BEGIN_SRC ")
  (insert language)
  (insert "\n\n#+END_SRC")
  (previous-line))

(defun launch-kitty-in-cwd ()
  (interactive)
  (let ((project-root (cdr (project-current))))
    (call-process "kitty" nil 0 nil "-d" default-directory)))

(defun launch-kitty-in-vc-root ()
  (interactive)
  (let ((project-root (cdr (project-current))))
    (call-process "kitty" nil 0 nil "-d" project-root)))

(defun launch-iterm-in-vc-root ()
  (interactive)
  (let ((project-root (cdr (project-current))))
    (shell-command (concat "open -a iTerm " project-root) nil nil)))

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

(defun ar/prefilled-swiper ()
  "Pre-fill swiper input with region."
  (interactive)
  (if (region-active-p)
      (let ((region-text (buffer-substring (region-beginning)
                                           (region-end))))
        (swiper region-text))
    (swiper)))

(defun ar/prefilled-swiper-backward ()
  "Pre-fill swiper input with region."
  (interactive)
  (if (region-active-p)
      (let ((region-text (buffer-substring (region-beginning)
                                           (region-end))))
        (swiper-backward region-text))
    (swiper-backward)))

(defun change-theme (&rest args)
  "Like `load-theme', but disables all themes before loading the new one."
  ;; The `interactive' magic is for creating a future-proof passthrough (see <https://emacs.stackexchange.com/a/19242>).
  (interactive (advice-eval-interactive-spec
                (cadr (interactive-form #'load-theme))))
  (mapcar #'disable-theme custom-enabled-themes)
  (apply (if (called-interactively-p 'any) #'funcall-interactively #'funcall)
         #'load-theme args))

(defun replace-string-with (newval)
  "https://stackoverflow.com/a/4925243/5067724"
  (interactive)
  (let ((my-quoted-string-regexp "\"\\(\\\\[\\\\\"]\\|[^\\\\\"]\\)*\""))
    (replace-regexp my-quoted-string-regexp newval)))


(defun user-flycheck-keybindings ()
  (local-set-key (kbd "S-<f2>") 'flycheck-previous-error)
  (local-set-key (kbd "<f2>") 'flycheck-next-error))

(add-hook 'flycheck-mode-hook		#'user-flycheck-keybindings)

(defun user-progmode-keybindings ()
  (local-set-key (kbd "C-c C-l") 'org-store-link))

(add-hook 'prog-mode-hook		#'user-progmode-keybindings)

(defun user-flymake-keybindings ()
  (local-set-key (kbd "S-<f2>") 'flymake-goto-prev-error)
  (local-set-key (kbd "<f2>") 'flymake-goto-next-error))
(add-hook 'flymake-mode-hook		#'user-flymake-keybindings)

(defun user-eglot-keybindings ()
  (global-set-key (kbd "H-a f") 'eglot-code-action-quickfix))
(add-hook 'eglot-mode-hook #'user-eglot-keybindings)

(defun rebl-eval-last-sexp ()
  (interactive)
  (let* ((bounds (cider-last-sexp 'bounds))
         (s (cider-last-sexp))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))

;; Similar to C-M-x, but sends to REBL
(defun rebl-eval-defun-at-point ()
  (interactive)
  (let* ((bounds (cider-defun-at-point 'bounds))
         (s (cider-defun-at-point))
         (reblized (concat "(cognitect.rebl/inspect " s ")")))
    (cider-interactive-eval reblized nil bounds (cider--nrepl-print-request-map))))


(defun select-and-copy-between-parens ()
  (interactive)
  (mark-sexp)
  (copy-region-as-kill (region-beginning) (region-end)))

(defun select-line ()
  (interactive)
  (back-to-indentation)
  (set-mark-command)
  (move-end-of-line))

(defun org-mode-<>-syntax-fix (start end)
  (when (eq major-mode 'org-mode)
    (save-excursion
      (goto-char start)
      (while (re-search-forward "<\\|>" end t)
    (when (get-text-property (point) 'src-block)
      ;; This is a < or > in an org-src block
      (put-text-property (point) (1- (point))
                         'syntax-table (string-to-syntax "_")))))))

(defun copy-parent-dir-as-kill ()
  (interactive)
  (kill-new (expand-file-name default-directory)))

(defun java-eval-nofocus ()
  "run current program (that requires no input)"
  (interactive)
  (let* ((source (file-name-nondirectory buffer-file-name))
     (out    (file-name-sans-extension source))
     (class  (concat out ".class")))
    (save-buffer)
    (shell-command (format "rm -f %s && javac %s" class source))
    (if (file-exists-p class)
    (shell-command (format "java %s" out) "*scratch*")
      (progn
    (set (make-local-variable 'compile-command)
         (format "javac %s" source))
    (command-execute 'compile)))))

(global-unset-key (kbd "C-z"))
(global-unset-key (kbd "C-x C-l"))

(global-set-key (kbd "M-o") #'split-window-right)
(global-set-key (kbd "M-O") #'delete-other-windows)
(global-set-key (kbd "C-S-o") #'delete-other-windows)
(global-set-key (kbd "C-o") #'other-window)
(global-set-key (kbd "C-S-t") #'launch-iterm-in-vc-root)
(global-set-key (kbd "C-;") #'comment-region)
(global-set-key [f2] nil)
(global-set-key (kbd "<next>") 'View-scroll-half-page-forward)
(global-set-key (kbd "<prior>") 'View-scroll-half-page-backward)
(global-set-key (kbd "C-,") 'project-find-file)
(global-set-key (kbd "C-.") 'project-find-regexp)
(global-set-key (kbd "C-\\") 'project-switch-to-buffer)
(global-set-key (kbd "C-x b") 'counsel-switch-buffer)
(global-set-key (kbd "C-x B") 'counsel-switch-buffer-other-window)
(global-set-key (kbd "M-p") (lambda () (interactive) (exchange-point-and-mark) (keyboard-quit)))
(global-set-key (kbd "C-`") #'er-switch-to-previous-buffer)
(global-set-key (kbd "M-`") #'other-frame)
(global-set-key (kbd "C-s") #'ar/prefilled-swiper)
(global-set-key (kbd "C-r") #'ar/prefilled-swiper-backward)
(global-set-key (kbd "C-1") #'set-mark-command)
(global-set-key (kbd "H-n") #'cua-rectangle-mark-mode)
(global-set-key (kbd "H-a") #'back-to-indentation)
(global-set-key (kbd "<C-left>") #'back-to-indentation)
(global-set-key (kbd "<C-right>") #'move-end-of-line)


(global-set-key (kbd "H-w")
   (lambda ()
      (interactive)
      (kill-new (thing-at-point 'symbol))))



;;; scroll like vim
(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(use-package clj-deps-new
  :ensure t)

;;; backup/autosave
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq-default cursor-type 'box)
(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
(setq-default indent-tabs-mode nil)
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))
(setq recentf-max-saved-items 100)
(setq confirm-kill-emacs 'yes-or-no-p)
(setq visible-cursor nil)
(setq nrepl-use-ssh-fallback-for-remote-hosts t)
(setq custom-safe-themes t)
(setq split-height-threshold nil) ;; only open horizontal splits (theoretically)
(setq visible-cursor nil)
(setq nrepl-use-ssh-fallback-for-remote-hosts t)
(setq org-babel-lisp-eval-fn #'sly-eval)
(setq org-babel-clojure-backend 'cider)
(setq set-mark-command-repeat-pop t) ;;; cycle thru marks w/ c-space
(setq mouse-wheel-progressive-speed nil) ;;; turn off mouse acceleration
(setq read-file-name-completion-ignore-case t) ;;; case insenstive autocompletion
(setq lsp-headerline-breadcrumb-enable nil)
(setq org-startup-folded t)

;;; get rid of blinking cursor
(blink-cursor-mode 0)

;; Escape C-x and C-c in terminal mode
(add-hook 'term-mode-hook (lambda ()
                            ;; Hack to set two escape chars.
                            (let (term-escape-char)
                              (term-set-escape-char ?\C-x))
                            (let (term-escape-char)
                              (term-set-escape-char ?\C-c))))

(define-key emacs-lisp-mode-map (kbd "<f5>") 'eval-buffer)

(add-hook 'c++-mode-hook (lambda () (c-toggle-comment-style 1)))

(add-hook 'org-mode-hook
      (lambda ()
        (setq syntax-propertize-function 'org-mode-<>-syntax-fix)
        (syntax-propertize (point-max))))

(add-hook 'rust-mode-hook 'electric-pair-mode)
(add-hook 'zig-mode-hook 'electric-pair-mode)
(add-hook 'c++-mode-hook 'electric-pair-mode)



;; ;; bind command to control on mac
(if (eq system-type 'darwin)
    (progn
      (setq mac-command-modifier 'control)
      (setq mac-control-modifier 'hyper)
      (global-set-key (kbd "H-s") #'save-some-buffers)))

;; ;; Set default font
;; ;; (set-face-font 'default "SF Mono:size=12")
;; ;; (set-face-font 'default "Menlo:size=10")
;; ;; (set-face-font 'default "Inconsolata:size=13")
;; ;; the russians make good fonts
(set-face-font 'default "Fira Mono:size=12")
;; (set-face-font 'default "Jetbrains Mono:size=12")
;; (set-face-font 'default "Terminus")

;;; org mode code eval
(org-babel-do-load-languages
      'org-babel-load-languages
      '((js . t)
        (lisp . t)
        (clojure . t)))

(setq org-startup-with-inline-images t)

;;; toolbar visibility
(tool-bar-mode -1)
(toggle-scroll-bar -1)

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize)
  (when (memq window-system '(mac ns))
    (setenv "PATH" (concat "/opt/homebrew/bin/:" (getenv "PATH")))))


(use-package ivy
  :ensure t
  :init (ivy-mode 1)
  :config
  (setq ivy-initial-inputs-alist nil)
  (setq ivy-use-virtual-buffers t)
  (setq ivy-re-builders-alist
        '((counsel-git-grep . ivy--regex-plus)
          (counsel-ag       . ivy--regex-plus)
          (counsel-rg       . ivy--regex-plus)
          ;; (counsel-find-file . ivy--regex-plus)
          (swiper           . ivy--regex-plus)
          (t                . ivy--regex-fuzzy)))
    (global-set-key (kbd "C-c i") 'counsel-imenu))

(ivy-configure 'counsel-imenu
  :update-fn 'auto)

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

(use-package ivy-clojuredocs
  :ensure t
  :bind (:map clojure-mode-map
              (("C-c d" . ivy-clojuredocs-at-point))))

(use-package counsel
  :ensure t
  :bind (("M-x"     . counsel-M-x)
         ;; ("C-s"     . swiper)
         ("C-x C-f" . counsel-find-file)
         ("C-x C-r" . counsel-recentf)  ; search for recently edited
         ("C-c g"   . counsel-git)      ; search for files in git repo
         ("C-c j"   . counsel-git-grep) ; search for regexp in git repo
         ("C-c /"   . counsel-rg)      ; Use ag for regexp
         ("C-c C-/"   . counsel-at-point-rg)
         ("M-y"     . counsel-yank-pop)))

(use-package counsel-at-point
  :ensure t)

(use-package magit
  :ensure t
  :bind (("C-x g" . magit-status)
         ("C-x C-g" . magit-status)))

(use-package undo-tree
  :ensure t
  :init
  (global-undo-tree-mode))

(use-package paren
  :config
  (show-paren-mode +1))

(use-package company
  :ensure t
  :hook
  (prog-mode . company-mode)
  (racket-repl-mode . company-mode)
  (sly-mode . company-mode)
  ;; (slime-mode . company-mode)
  (cider-repl-mode . company-mode))

(use-package racket-mode
  :ensure t
  :hook (racket-mode . racket-xp-mode))

(use-package flycheck
  :ensure t
  ;; :disabled t
  :bind (("<f2>" . flycheck-next-error)
         ("S-<f2>" . flycheck-previous-error)))

(use-package flycheck-clj-kondo
  ;; :disabled t
  :ensure t)

(use-package paredit
  ;; :defer t
  :ensure t
  :bind (("M-[" . paredit-wrap-square)
         ("M-{" . paredit-wrap-curly)
         ("C-M-{" . backward-paragraph)
         ("C-M-}" . forward-paragraph))
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'clojurescript-mode-hook 'paredit-mode)
    ;; add-hook 'clojure
    (add-hook 'cider-repl-mode-hook 'paredit-mode)
    (add-hook 'racket-mode-hook 'paredit-mode)
    (add-hook 'racket-repl-mode-hook 'paredit-mode))
  (bind-key "C-M-w" 'select-and-copy-between-parens)
  :hook
  (sly-mode . paredit-mode))

(use-package clojure-mode
  :ensure t
  :config
  ;; (require 'flycheck-clj-kondo)
  :hook (clojure-mode . flycheck-mode)
  :hook (clojure-mode . inf-clojure-minor-mode)
  )

;; Similar to C-x C-e, but sends to REBL


(use-package cider
  ;; :defer t
  :ensure t
  :bind (("C-c =" . cider-format-buffer)
         ("C-." . cider-find-dwim))
  :init
  (progn
    ;; (add-hook 'clojure-mode-hook 'cider-mode)
    (add-hook 'clojurescript-mode-hook 'cider-mode)
    (add-hook 'clojurec-mode-hook 'cider-mode)
    (add-hook 'cider-repl-mode-hook 'cider-mode))
  :config
  (setq cider-repl-display-help-banner nil)
  (setq cider-auto-mode t)
  (setq show-paren-mode t)
  (setq cider-repl-pop-to-buffer-on-connect nil))


;; (add-hook 'swift-mode-hook #'flycheck-mode)

;;; gdb setup
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t)

(use-package sly-quicklisp
  :after sly
  :ensure t)

(use-package sly-asdf
  :after sly
  :ensure t)

(use-package sly
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

;; (add-hook 'dart-mode-hook 'lsp)
;; (add-hook 'dart-mode-hook (lambda () (lsp-mode +1)))

(setq gc-cons-threshold (* 100 1024 1024)
      read-process-output-max (* 1024 1024)
      company-minimum-prefix-length 1
      lsp-lens-enable t
      lsp-signature-auto-activate nil)
;;; lsp

(use-package eglot
  :ensure t
  :disabled t)



;; (use-package lsp-mode
;;   :ensure t
;;   :disabled t
;;   ;; :hook (dart-mode . lsp-mode)
;;   )

(use-package lsp-mode
  :ensure
  :commands lsp
  :custom
  ;; what to use when checking on-save. "check" is default, I prefer clippy
  (lsp-rust-analyzer-cargo-watch-command "clippy")
  ;; (lsp-idle-delay 0.6)
  (lsp-rust-analyzer-server-display-inlay-hints t)
  ;; :config  (add-hook 'lsp-mode-hook 'lsp-ui-mode)
      )

(use-package lsp-ui
  :ensure t
  :commands lsp-ui-mode
  :custom
  (lsp-ui-peek-always-show nil)
  (lsp-ui-sideline-show-hover nil)
  (lsp-ui-sideline-show-code-actions t)
  ;; (lsp-ui-doc-show-with-mouse nil)
  ;; (lsp-ui-doc-enable nil)
  ;; (lsp-enable-symbol-highlighting t)
  ;; (lsp-lens-enable nil)
  ;; (lsp-eldoc-enable-hover nil)
  ;; (lsp-signature-auto-activate nil)
  ;; (lsp-signature-render-documentation nil)
  ;; (lsp-headerline-breadcrumb-enable nil)
  :config
  (setq lsp-ui-sideline-actions-kind-regex ".*")
  )

;; (use-package lsp-ui
;;   ;; :init (add-hook 'lsp-mode-hook 'lsp-ui-mode)
;;   :ensure t
;;   :disabled t
;;   :hook (lsp-mode . lsp-ui-mode))

(use-package org-download
  :ensure t)

(use-package cargo
  :ensure t)

(use-package rustic
  :ensure
  :bind (:map rustic-mode-map
              ("M-j" . lsp-ui-imenu)
              ("M-?" . lsp-find-references)
              ("C-c C-c l" . flycheck-list-errors)
              ("C-!" . flycheck-explain-error-at-point)
              ("C-c C-c a" . lsp-execute-code-action)
              ("C-c C-c r" . lsp-rename)
              ("C-c C-c q" . lsp-workspace-restart)
              ("C-c C-c Q" . lsp-workspace-shutdown)
              ("C-c C-c s" . lsp-rust-analyzer-status)
              ("C-c C-c d" . lsp-describe-thing-at-point))
  :config
  ;; uncomment for less flashiness
  (setq lsp-eldoc-hook nil)
  ;; (setq lsp-enable-symbol-highlighting nil)
  (setq lsp-signature-auto-activate nil)
 
  ;; comment to disable rustfmt on save
  (setq rustic-format-on-save t)
  (add-hook 'rustic-mode-hook 'je/rustic-mode-hook))

(use-package yasnippet
  :ensure
  :config
  (yas-reload-all)
  (add-hook 'prog-mode-hook 'yas-minor-mode)
  (add-hook 'text-mode-hook 'yas-minor-mode))

(use-package expand-region
  :bind ("C-=" . er/expand-region))

(use-package license-templates
  :ensure t)

;; (require 'license-templates)

(use-package expand-region
  :ensure t)

(use-package zig-mode
  :ensure t)

(defun je/rustic-mode-hook ()
  ;; so that run C-c C-c C-r works without having to confirm, but don't try to
  ;; save rust buffers that are not file visiting. Once
  ;; https://github.com/brotzeit/rustic/issues/253 has been resolved this should
  ;; no longer be necessary.
  (when buffer-file-name
    (setq-local buffer-save-without-query t)))


;; ########################## Custom

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(lsp-lsp-flycheck-warning-unnecessary-face ((t (:inherit modus-themes-lang-warning :foreground "dim gray"))) t))
'(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(custom-safe-themes
   '("51ec7bfa54adf5fff5d466248ea6431097f5a18224788d0bd7eb1257a4f7b773" "e0d42a58c84161a0744ceab595370cbe290949968ab62273aed6212df0ea94b4" "96998f6f11ef9f551b427b8853d947a7857ea5a578c75aa9c4e7c73fe04d10b4" "9b59e147dbbde5e638ea1cde5ec0a358d5f269d27bd2b893a0947c4a867e14c1" "b3775ba758e7d31f3bb849e7c9e48ff60929a792961a2d536edec8f68c671ca5" "3d5ef3d7ed58c9ad321f05360ad8a6b24585b9c49abcee67bdcbb0fe583a6950" "3cd28471e80be3bd2657ca3f03fbb2884ab669662271794360866ab60b6cb6e6" "e9776d12e4ccb722a2a732c6e80423331bcb93f02e089ba2a4b02e85de1cf00e" "72a81c54c97b9e5efcc3ea214382615649ebb539cb4f2fe3a46cd12af72c7607" "987b709680284a5858d5fe7e4e428463a20dfabe0a6f2a6146b3b8c7c529f08b" "58c6711a3b568437bab07a30385d34aacf64156cc5137ea20e799984f4227265" "285d1bf306091644fb49993341e0ad8bafe57130d9981b680c1dbd974475c5c7" "3cc2385c39257fed66238921602d8104d8fd6266ad88a006d0a4325336f5ee02" "c48551a5fb7b9fc019bf3f61ebf14cf7c9cdca79bcb2a4219195371c02268f11" "00445e6f15d31e9afaa23ed0d765850e9cd5e929be5e8e63b114a3346236c44c" "efcecf09905ff85a7c80025551c657299a4d18c5fcfedd3b2f2b6287e4edd659" "6bdcff29f32f85a2d99f48377d6bfa362768e86189656f63adbf715ac5c1340b" "4eb6fa2ee436e943b168a0cd8eab11afc0752aebb5d974bba2b2ddc8910fca8f" "830877f4aab227556548dc0a28bf395d0abe0e3a0ab95455731c9ea5ab5fe4e1" "4c56af497ddf0e30f65a7232a8ee21b3d62a8c332c6b268c81e9ea99b11da0d3" "fee7287586b17efbfda432f05539b58e86e059e78006ce9237b8732fde991b4c" "f5b6be56c9de9fd8bdd42e0c05fecb002dedb8f48a5f00e769370e4517dde0e8" "7f1d414afda803f3244c6fb4c2c64bea44dac040ed3731ec9d75275b9e831fe5" "83e0376b5df8d6a3fbdfffb9fb0e8cf41a11799d9471293a810deb7586c131e6" "06f0b439b62164c6f8f84fdda32b62fb50b6d00e8b01c2208e55543a6337433a" "1b8d67b43ff1723960eb5e0cba512a2c7a2ad544ddb2533a90101fd1852b426e" "ea78e2f43f093d5ea4ff43cbd58e75ff66e13c3ce20d5f5e0c977d8382b4f18b" "8f0a782ba26728fa692d35e82367235ec607d0c836e06bc39eb750ecc8e08258" "171d1ae90e46978eb9c342be6658d937a83aaa45997b1d7af7657546cae5985b" "d14f3df28603e9517eb8fb7518b662d653b25b26e83bd8e129acea042b774298" "5185a285365a768a30ac274bdbc4437e7fd2fbe3107a1b0f2b60e900181905e0" "6b5c518d1c250a8ce17463b7e435e9e20faa84f3f7defba8b579d4f5925f60c1" "7661b762556018a44a29477b84757994d8386d6edee909409fabe0631952dad9" "c1284dd4c650d6d74cfaf0106b8ae42270cab6c58f78efc5b7c825b6a4580417" "0c3b1358ea01895e56d1c0193f72559449462e5952bded28c81a8e09b53f103f" "376f42fd34de7bfdb4b6cc6f27bc271881eb5757f92e428260083412f036ef52" "1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "b0e446b48d03c5053af28908168262c3e5335dcad3317215d9fdeb8bac5bacf9" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "4133d2d6553fe5af2ce3f24b7267af475b5e839069ba0e5c80416aa28913e89a" "6c386d159853b0ee6695b45e64f598ed45bd67c47f671f69100817d7db64724d" "e6f3a4a582ffb5de0471c9b640a5f0212ccf258a987ba421ae2659f1eaa39b09" "745d03d647c4b118f671c49214420639cb3af7152e81f132478ed1c649d4597d" "b9a67b48d56c580cb300ce9c1ecc3b83aee953346a33e2a14b31e2e4a07ea8a6" "e3a1b1fb50e3908e80514de38acbac74be2eb2777fc896e44b54ce44308e5330" "b6269b0356ed8d9ed55b0dcea10b4e13227b89fd2af4452eee19ac88297b0f99" "b02eae4d22362a941751f690032ea30c7c78d8ca8a1212fdae9eecad28a3587f" "da53441eb1a2a6c50217ee685a850c259e9974a8fa60e899d393040b4b8cc922" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "6b1abd26f3e38be1823bd151a96117b288062c6cde5253823539c6926c3bb178" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "c8b83e7692e77f3e2e46c08177b673da6e41b307805cd1982da9e2ea2e90e6d7" "24168c7e083ca0bbc87c68d3139ef39f072488703dcdd82343b8cab71c0f62a7" "fb83a50c80de36f23aea5919e50e1bccd565ca5bb646af95729dc8c5f926cbf3" "a3bdcbd7c991abd07e48ad32f71e6219d55694056c0c15b4144f370175273d16" "39fe48be738ea23b0295cdf17c99054bb439a7d830248d7e6493c2110bfed6f8" "7beac4a68f03662b083c9c2d4f1d7f8e4be2b3d4b0d904350a9edf3cf7ce3d7f" "b47eca77c785108ab443aea40fbabb2af3e13a3ac8a8537975dee099b866a0f0" "8ecc63db23c4b608a2ba60974c8fe7a0f8c57d8563f83c5c126f002d6ae2d48e" "e8a0c94af8c0eeec7ae0f1633d29098ea722e5765f1e9c67b49da6f3414b9bfe" "c3957b559cf3606c9a40777c5712671db3c7538e5d5ea9f63eb0729afeac832b" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "93268bf5365f22c685550a3cbb8c687a1211e827edc76ce7be3c4bd764054bad" "5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" "96be1c5bb74fc2ffdfed87e46c87f1492969bf2af1fc96232e35c06b517aecc1" "8c1dd3d6fdfb2bee6b8f05d13d167f200befe1712d0abfdc47bb6d3b706c3434" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "36282815a2eaab9ba67d7653cf23b1a4e230e4907c7f110eebf3cdf1445d8370" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "1f538e3de6aa8711e3ad8f297a57ee8210ade0f46f411c06343d126a4fea72c6" "bc4c89a7b91cfbd3e28b2a8e9e6750079a985237b960384f158515d32c7f0490" "b6493078dc17528c57a3eb6b334e496217d026d856261b349603c9845f3cf2d8" "1bb8f76bcd04a2b25a663a3da69235fbdbe9db1d5fe7efc6e8fcfc5e1030c9c3" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "c560237b7505f67a271def31c706151afd7aa6eba9f69af77ec05bde5408dbcd" "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2" "d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c" "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7" "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" "23b564cfb74d784c73167d7de1b9a067bcca00719f81e46d09ee71a12ef7ee82" "df4a201edd54dbce57d029c3f1d91ccbf3a06ce22365c0d00d669442fcb2bced" "04a31c2a707e2ca1cb5c411a3b9e183760726c9d61983f58d95ce37c1c97d086" "ac6e2b8f5c58c4710f59b7d2652bab7b715476696b05f5395f6b5cdd64e41160" "18cd5a0173772cdaee5522b79c444acbc85f9a06055ec54bb91491173bc90aaa" "5d59bd44c5a875566348fa44ee01c98c1d72369dc531c1c5458b0864841f887c" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "5379937b99998e0510bd37ae072c7f57e26da7a11e9fb7bced8b94ccc766c804" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "0685ffa6c9f1324721659a9cd5a8931f4bb64efae9ce43a3dba3801e9412b4d8" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "d5a878172795c45441efcd84b20a14f553e7e96366a163f742b95d65a3f55d71" "cae81b048b8bccb7308cdcb4a91e085b3c959401e74a0f125e7c5b173b916bf9" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "e3c64e88fec56f86b49dcdc5a831e96782baf14b09397d4057156b17062a8848" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "2c49d6ac8c0bf19648c9d2eabec9b246d46cb94d83713eaae4f26b49a8183fc4" "0a41da554c41c9169bdaba5745468608706c9046231bbbc0d155af1a12f32271" "e27556a94bd02099248b888555a6458d897e8a7919fd64278d1f1e8784448941" "ff3c57a5049010a76de8949ddb629d29e2ced42b06098e046def291989a4104a" "f4876796ef5ee9c82b125a096a590c9891cec31320569fc6ff602ff99ed73dca" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "0fe24de6d37ea5a7724c56f0bb01efcbb3fe999a6e461ec1392f3c3b105cc5ac" "4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab" "4f01c1df1d203787560a67c1b295423174fd49934deb5e6789abd1e61dba9552" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "c086fe46209696a2d01752c0216ed72fd6faeabaaaa40db9fc1518abebaf700d" "56d10d2b60685d112dd54f4ba68a173c102eacc2a6048d417998249085383da1" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "fd22c8c803f2dac71db953b93df6560b6b058cb931ac12f688def67f08c10640" "f94110b35f558e4c015b2c680f150bf8a19799d775f8352c957d9d1054b0a210" "75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" "ca70827910547eb99368db50ac94556bbd194b7e8311cfbdbdcad8da65e803be" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "fce3524887a0994f8b9b047aef9cc4cc017c5a93a5fb1f84d300391fba313743" "95d0ed21bb0e919be7687a25ad59a1c2c8df78cbe98c9e369d44e65bfd65b167" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "0e2a7e1e632dd38a8e0227d2227cb8849f877dd878afb8219cb6bcdd02068a52" "7546a14373f1f2da6896830e7a73674ef274b3da313f8a2c4a79842e8a93953e" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "d9646b131c4aa37f01f909fbdd5a9099389518eb68f25277ed19ba99adeb7279" "d9495c98266e15a77c3cd6cb45f7964891c189cf613337d9a2e2950881493c09" default))

 '(flycheck-color-mode-line-face-to-color 'mode-line-buffer-id)
 '(haskell-mode-hook '(interactive-haskell-mode))
 '(haskell-mode-stylish-haskell-path "fourmolu")
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-stylish-on-save nil)
 '(haskell-w3m-haddock-dirs '("~/.ghcup/share/doc/"))
 '(inhibit-startup-screen t)
 '(line-number-mode t)
 '(linum-format " %5i ")
 '(lsp-auto-guess-root t)
 '(lsp-dart-flutter-widget-guides nil)
 ;; '(lsp-eldoc-enable-hover t)
 ;; '(lsp-eldoc-render-all nil)
 ;; '(lsp-enable-file-watchers nil)
 ;; '(lsp-enable-indentation t t)
 ;; '(lsp-haskell-format-on-import-on t)
 ;; '(lsp-haskell-formatting-provider "ormolu")
 ;; '(lsp-haskell-hlint-on nil)
 ;; '(lsp-ui-doc-border "#586e75")
 ;; '(lsp-ui-doc-enable t)
 ;; '(lsp-ui-peek-enable t)
 ;; '(org-src-block-faces 'nil)
 '(package-selected-packages
   '(lispy sublime-themes project solarized-theme helm-rg tramp helm-tramp hasklig-mode ligature atom-one-dark-theme dracula-theme gruvbox-theme jetbrains-darcula-theme markdown-mode markdown-preview-mode elixir elixer-mode company-ghci tree-sitter-indent fzf monokai-pro-theme vscode-dark-plus-theme evil nord-theme csv-mode mood-one-theme nothing-theme phoenix-dark-mono-theme punpun-theme quasi-monochrome-theme spacegray-theme pkgbuild-mode flutter almost-mono-themes sexy-monochrome-theme purp-theme prassee-theme plan9-theme naysayer-theme company lsp-ui spacemacs-theme cyberpunk-theme lsp-haskell rmsbolt peep-dired flycheck w3m exec-path-from-shell python-mode nix-mode racket-mode function-args haskell-mode helm-slime slime elpher fish-mode cider paredit clojure-mode helm lsp-mode magit zig-mode yaml-mode meson-mode))
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-very-old-color nil)
 '(vc-follow-symlinks t)
 '(window-divider-mode nil)) 

;;; needs to be after config


;;;(add-hook 'haskell-literate-mode-hook 'lsp)
;;;(add-hook 'rust-mode-hook 'lsp)
(setq haskell-process-type 'stack-ghci)
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline success warning error])
 '(awesome-tray-mode-line-active-color "#0031a9")
 '(awesome-tray-mode-line-inactive-color "#d7d7d7")
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(custom-enabled-themes '(modus-operandi))
 '(custom-safe-themes
   '("8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" default))
 '(exwm-floating-border-color "#888888")
 '(fci-rule-color "#555556")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-themes-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-themes-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-themes-fringe-yellow))
 '(highlight-tail-colors ((("#333a23") . 0) (("#2d3936") . 20)))
 '(hl-todo-keyword-faces
   '(("HOLD" . "#70480f")
     ("TODO" . "#721045")
     ("NEXT" . "#5317ac")
     ("THEM" . "#8f0075")
     ("PROG" . "#00538b")
     ("OKAY" . "#30517f")
     ("DONT" . "#315b00")
     ("FAIL" . "#a60000")
     ("BUG" . "#a60000")
     ("DONE" . "#005e00")
     ("NOTE" . "#863927")
     ("KLUDGE" . "#813e00")
     ("HACK" . "#813e00")
     ("TEMP" . "#5f0000")
     ("FIXME" . "#a0132f")
     ("XXX+" . "#972500")
     ("REVIEW" . "#005a5f")
     ("DEPRECATED" . "#201f55")))
 '(ibuffer-deletion-face 'modus-themes-mark-del)
 '(ibuffer-filter-group-name-face 'modus-themes-pseudo-header)
 '(ibuffer-marked-face 'modus-themes-mark-sel)
 '(ibuffer-title-face 'default)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#FD971F"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#A6E22E"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#525254"))
 '(linum-format " %7i ")
 '(magit-diff-use-overlays nil)
 '(nrepl-message-colors
   '("#ffb4ac" "#ddaa6f" "#e5c06d" "#3d464c" "#e3eaea" "#41434a" "#7ec98f" "#e5786d" "#834c98"))
 '(objed-cursor-color "#E74C3C")
 '(org-src-block-faces 'nil)
 '(package-selected-packages
   '(inf-clojure color-theme-sanityinc-solarized web-mode zig vterm license-templates lsp-ui expand-region yasnippet rustic autopair counsel-at-point nim-mode rust-mode cargo carge eglot flutter-l10n-flycheck flutter kaolin-themes solarized-theme org-download modus-themes dart-mode devdocs kotlin-mode flycheck-swift swift-mode evil multiple-cursors flycheck-swift3 counsel-fd eziam-theme tao-theme minimal-theme wgrep goto-last-point markdown-mode package-lint hydra hackernews company-shell company dash-docs ivy-lobsters dash-at-point simple-httpd counsel-ag-popup counsel-tramp smex timu-spacegrey-theme ivy-clojuredocs flx counsel srefactor nano-theme white-sand-theme leuven-theme exec-path-from-shell white-theme one-themes spacemacs-theme flycheck-clj-kondo sly-quicklisp sly-asdf sly espresso-theme chocolate-theme helm-company helm-sly danneskjold-theme undo-tree su tango-plus-theme rainbow-delimiters gotham-theme nimbus-theme mood-one-theme night-owl-theme zig-mode yaml-mode use-package sublime-themes racket-mode project paredit naysayer-theme monokai-pro-theme meson-mode markdown-preview-mode magit lua-mode lsp-haskell lsp-dart lispy helm-rg hasklig-mode gruvbox-theme flycheck fish-mode evil-surround elpher dracula-theme company-ghci cider almost-mono-themes))
 '(pdf-view-midnight-colors '("#000000" . "#f8f8f8"))
 '(pos-tip-background-color "#2f2f2e")
 '(pos-tip-foreground-color "#999891")
 '(rustic-ansi-faces
   ["#272822" "#E74C3C" "#A6E22E" "#E6DB74" "#268bd2" "#F92660" "#66D9EF" "#F8F8F2"])
 '(safe-local-variable-values
   '((cider-clojure-cli-global-options . "-A:dev")
     (inf-clojure-custom-repl-type . clojure)
     (inf-clojure-custom-startup "localhost" . 50505)
     (cider-ns-refresh-after-fn . "integrant.repl/resume")
     (cider-ns-refresh-before-fn . "integrant.repl/suspend")
     (cider-clojure-cli-global-options . "-A:reveal")))
 '(show-paren-mode t)
 '(smartrep-mode-line-active-bg (solarized-color-blend "#8ac6f2" "#2f2f2e" 0.2))
 '(tao-theme-use-boxes t)
 '(term-default-bg-color "#2a2a29")
 '(term-default-fg-color "#8d8b86")
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#a60000")
     (40 . "#721045")
     (60 . "#8f0075")
     (80 . "#972500")
     (100 . "#813e00")
     (120 . "#70480f")
     (140 . "#5d3026")
     (160 . "#184034")
     (180 . "#005e00")
     (200 . "#315b00")
     (220 . "#005a5f")
     (240 . "#30517f")
     (260 . "#00538b")
     (280 . "#093060")
     (300 . "#0031a9")
     (320 . "#2544bb")
     (340 . "#0000c0")
     (360 . "#5317ac")))
 '(vc-annotate-very-old-color nil)
 '(warning-suppress-types '((comp) (comp)))
 '(weechat-color-list
   '(unspecified "#2a2a29" "#2f2f2e" "#504341" "#ffb4ac" "#3d464c" "#8ac6f2" "#4c4536" "#e5c06d" "#41434a" "#a4b5e6" "#4d3936" "#e5786d" "#3b473c" "#7ec98f" "#8d8b86" "#74736f"))
 '(window-divider-mode nil)
 '(xterm-color-names
   ["black" "#a60000" "#005e00" "#813e00" "#0031a9" "#721045" "#00538b" "gray65"])
 '(xterm-color-names-bright
   ["gray35" "#972500" "#315b00" "#70480f" "#2544bb" "#8f0075" "#30517f" "white"]))

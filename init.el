(setq-default indent-tabs-mode nil)
(require 'lsp-mode)

;;; js org mode
(org-babel-do-load-languages
      'org-babel-load-languages
      '((js . t)))

;;; haskell setup

(require 'haskell-interactive-mode)
(require 'haskell-process)
;(require 'flymake-haskell-multi) ;; not needed if installed via package
(add-hook 'haskell-mode-hook 'interactive-haskell-mode)
(setq haskell-process-type 'stack-ghci)

;; dante
(add-hook 'haskell-mode-hook 'dante-mode)
(remove-hook 'flymake-diagnostic-functions 'flymake-proc-legacy-flymake)
(add-hook 'haskell-mode-hook 'flymake-mode)

;; lsp
;; (require 'lsp)
;; (require 'lsp-haskell)
;; (add-hook 'haskell-mode-hook #'lsp)
;; (add-hook 'haskell-literate-mode-hook #'lsp)

(add-hook 'haskell-mode-hook 'flymake-haskell-multi-load)
;(add-hook 'haskell-mode-hook 'haskell-setup)

;;; unbind page up and page down

(define-key (current-global-map) (kbd "<next>") nil) 
(global-unset-key (kbd "<prior>"))                   

;;; function args setup

;(fa-config-default)

;;; flymake setup

(defun user-flymake-keybindings ()
  (local-set-key (kbd "M-p") 'flymake-goto-prev-error)
  (local-set-key (kbd "M-n") 'flymake-goto-next-error)
)

(add-hook 'flymake-mode-hook		#'user-flymake-keybindings)


;;; racket setup

;;; slime setup
(setq inferior-lisp-program "sbcl")
(add-hook 'slime-repl-mode-hook (lambda () (paredit-mode +1)))

;;; initial buffer selection
(setq initial-buffer-choice "/home/solaire/notes/orgmode/todo.org")

;;; lisp setup

(paredit-mode 1)
(add-hook 'clojure-mode-hook #'paredit-mode)
(show-paren-mode 1)
(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
(add-hook 'racket-mode-hook           #'enable-paredit-mode)
(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;;; gdb setup
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t

 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t
 )

;;; open links in eww
;(setq browse-url-browser-function 'eww-browse-url)

;;; helm setup
(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-c h o") 'helm-occur)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

(helm-autoresize-mode t)
(setq helm-autoresize-max-height 20)
(setq helm-autoresize-min-height 20)
(setq helm-split-window-in-side-p t)

;;; helm-slime setup

(defun user-slime-repl-keybindings ()
  (local-set-key (kbd "C-c h y") 'helm-slime-repl-history)
)

(add-hook 'slime-repl-mode-hook		#'user-slime-repl-keybindings)

(helm-mode 1)

;;;; lsp setup
;;; prefix for LSP commands
(setq lsp-keymap-prefix "C-c C-o")


(add-hook 'c-mode-hook 'lsp)
(add-hook 'c++-mode-hook 'lsp)

(add-hook 'c++-mode-hook (lambda () (c-toggle-comment-style 1)))
;(setq lsp-signature-auto-activate t)

;;; stop indenting please
(setq lsp-enable-indentation nil)

;;; WHY DO I KEEP QUITTING
(setq confirm-kill-emacs 'yes-or-no-p)

;;; relative line numbers
(setq display-line-numbers-type 'relative)

;;; get rid of blinking cursor
(blink-cursor-mode 0)

;;; scroll like vim
(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

;;; jump to previous buffer
(defun er-switch-to-previous-buffer ()
  "Switch to previously open buffer.
Repeated invocations toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(global-set-key (kbd "C-x x") #'er-switch-to-previous-buffer)


;;; backup/autosave
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;;; #EXTERNAL DEPENDENCY universal ctags
;;; create-tags function
(setq path-to-ctags "/usr/bin/ctags")
(defun create-tags (dir-name)
  "Create tags file."
  (interactive "DDirectory: ")
  (shell-command (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name)))
  )

;;; toolbar visibility
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode 1)

;;; how the fuck do i deal with tabs 
;(setq tab-always-indent 'complete)
;(setq tab-width 4)
;(setq indent-tabs-mode nil)
;;(Setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;;(setq indent-line-function 'insert-tab)

;;; cycle thru marks w/ c-space
(setq set-mark-command-repeat-pop t)

;;; turn off mouse acceleration
(setq mouse-wheel-progressive-speed nil)

;;; case insenstive autocompletion
(setq read-file-name-completion-ignore-case t)

;;; c indentation
(setq c-default-style "stroustrup")
(setq c-offsets-alist '((arglist-cont-nonempty . +)))

;;; config org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;; jump to header in c file
(add-hook 'c-mode-common-hook
  (lambda() 
    (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

;;; configure melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
;(package-initialize)

;;; auto install packages at startup

(defvar my-packages
  '(lsp-haskell haskell-mode helm-slime slime elpher fish-mode cider paredit clojure-mode helm lsp-mode magit zig-mode yaml-mode meson-mode evil))

(require 'cl-lib)
;(package-initialize)
(unless (cl-every #'package-installed-p my-packages)
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (package-install package))))


;;; DEPENDENCY: fzf
;;; keybind for fzf
; (global-set-key (kbd "M-p") 'fzf-git)

;; ########################## DO NOT MODIFY
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   '("d9646b131c4aa37f01f909fbdd5a9099389518eb68f25277ed19ba99adeb7279" "d9495c98266e15a77c3cd6cb45f7964891c189cf613337d9a2e2950881493c09" default))
 '(display-line-numbers-type 'relative t)
 '(inhibit-startup-screen t)
 '(lsp-haskell-format-on-import-on t)
 '(lsp-haskell-formatting-provider "brittany")
 '(package-selected-packages
   '(dante python-mode nix-mode flymake-haskell-multi racket-mode function-args haskell-mode helm-slime slime elpher fish-mode cider paredit clojure-mode helm lsp-mode magit zig-mode yaml-mode meson-mode))
 '(show-paren-mode t)
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t))
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "SFMono Nerd Font Mono" :foundry "APPL" :slant normal :weight normal :height 128 :width normal)))))

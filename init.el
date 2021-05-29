;;; configure melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(defvar my-packages
  '(exec-path-from-shell flycheck lsp-haskell haskell-mode helm-slime slime elpher fish-mode cider paredit clojure-mode helm lsp-mode magit zig-mode yaml-mode meson-mode nix-mode))

(require 'cl-lib)
(package-initialize)
(unless (cl-every #'package-installed-p my-packages)
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (package-install package))))

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

(exec-path-from-shell-initialize)

(setq-default indent-tabs-mode nil)
;;; js org mode
(org-babel-do-load-languages
      'org-babel-load-languages
      '((js . t)))

;;; goddamn comments

(set-face-foreground 'font-lock-comment-face "grey")

;;; rust

(setq rust-format-on-save t)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
;;(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)

;;; haskell setup

(require 'haskell-interactive-mode)
(require 'haskell-process)

;;; dart setup

(setq dart-format-on-save t)

;;; lsp

(setq lsp-keymap-prefix "C-c C-f")

;; (defun user-haskell-save-hook ()
;;   (when (eq major-mode 'haskell-mode)
;;     (shell-command-to-string (format "brittany --write-mode inplace %s" buffer-file-name))
;;     (revert-buffer :ignore-auto :noconfirm)
;;     )
;;   ) 
;;(add-hook 'after-save-hook #'user-haskell-save-hook)
(setq haskell-mode-stylish-haskell-path "ormolu") 

(require 'flycheck)
;(add-hook 'haskell-mode-hook 'flycheck-mode)
;;(setq flycheck-display-errors-function nil)
 '(flycheck-check-syntax-automatically (quote (save idle-change mode-
enabled)))
 '(flycheck-idle-change-delay 4) ;; Set delay based on what suits you the best

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
;;(setq initial-buffer-choice "/home/solaire/notes/orgmode/todo.org")

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
;;(setq browse-url-browser-function 'eww-browse-url)

;;; helm setup
(require 'helm)
(require 'helm-config)

(global-set-key (kbd "C-x b") 'helm-mini)
(global-set-key (kbd "C-s") 'helm-occur)
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
(menu-bar-mode -1)

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

;(package-initialize)

;;; auto install packages at startup

;; show columns

(setq column-number-mode t)
;; ########################## Custom

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Iosevka" :foundry "BE5N" :slant normal :weight normal :height 158 :width normal))))
 '(lsp-ui-doc-background ((t (:inherit tooltip))))
 '(lsp-ui-sideline-symbol ((t (:foreground "grey" :box (:line-width (1 . -1) :color "grey") :height 0.99))))
 '(lsp-ui-sideline-symbol-info ((t (:extend t)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-faces-vector
   [default bold shadow italic underline success warning error])
 '(ansi-color-names-vector
   ["#000000" "#ff6c6b" "#98be65" "#ECBE7B" "#0170bf" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-safe-themes
 '(display-line-numbers-type 'relative t)
 '(exwm-floating-border-color "#646464")
 '(fci-rule-color "#5B6268")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(haskell-mode-hook '(interactive-haskell-mode))
 '(haskell-mode-stylish-haskell-path "ormolu" t)
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-stylish-on-save t)
 '(haskell-w3m-haddock-dirs '("~/.ghcup/share/doc/"))
 '(highlight-tail-colors '(("#2f4a00" . 0) ("#00415e" . 20)))
 '(hl-paren-background-colors '("#e8fce8" "#c1e7f8" "#f8e8e8"))
 '(hl-paren-colors '("#40883f" "#0287c8" "#b85c57"))
 '(hl-sexp-background-color "#efebe9")
 '(hl-todo-keyword-faces
   '(("TODO" . "#dc752f")
     ("NEXT" . "#dc752f")
     ("THEM" . "#2d9574")
     ("PROG" . "#4f97d7")
     ("OKAY" . "#4f97d7")
     ("DONT" . "#f2241f")
     ("FAIL" . "#f2241f")
     ("DONE" . "#86dc2f")
     ("NOTE" . "#b1951d")
     ("KLUDGE" . "#b1951d")
     ("HACK" . "#b1951d")
     ("TEMP" . "#b1951d")
     ("FIXME" . "#dc752f")
     ("XXX+" . "#dc752f")
     ("\\?\\?\\?+" . "#dc752f")))
 '(ibuffer-deletion-face 'modus-theme-mark-del)
 '(ibuffer-filter-group-name-face 'modus-theme-mark-symbol)
 '(ibuffer-marked-face 'modus-theme-mark-sel)
 '(ibuffer-title-face 'modus-theme-pseudo-header)
 '(inhibit-startup-screen t)
 '(jdee-db-active-breakpoint-face-colors (cons "#1B2229" "#0170bf"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#1B2229" "#98be65"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#1B2229" "#5B6268"))
 '(linum-format " %5i ")
 '(lsp-auto-guess-root t)
 '(lsp-dart-flutter-widget-guides nil)
 '(lsp-eldoc-enable-hover t)
 '(lsp-eldoc-render-all nil)
 '(lsp-enable-file-watchers nil)
 '(lsp-enable-indentation t t)
 '(lsp-haskell-format-on-import-on t)
 '(lsp-haskell-formatting-provider "ormolu")
 '(lsp-signature-render-documentation nil)
 '(lsp-ui-doc-alignment 'window)
 '(lsp-ui-doc-enable t)
 '(lsp-ui-doc-show-with-cursor nil)
 '(lsp-ui-peek-enable t)
 '(lsp-ui-peek-list-width 20)
 '(lsp-ui-sideline-show-code-actions nil)
 '(lsp-ui-sideline-show-symbol nil)
 '(objed-cursor-color "#ff6c6b")
 '(org-src-block-faces 'nil)
 '(package-selected-packages
   '(csv-mode lsp-dart mood-one-theme nothing-theme phoenix-dark-mono-theme punpun-theme quasi-monochrome-theme spacegray-theme pkgbuild-mode dart-mode flutter almost-mono-themes sexy-monochrome-theme purp-theme prassee-theme plan9-theme naysayer-theme company lsp-ui spacemacs-theme doom-themes cyberpunk-theme lsp-haskell racer rust-mode rmsbolt peep-dired flycheck w3m exec-path-from-shell python-mode nix-mode racket-mode function-args haskell-mode helm-slime slime elpher fish-mode cider paredit clojure-mode helm lsp-mode magit zig-mode yaml-mode meson-mode))
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#000000"))
 '(rustic-ansi-faces
   ["#000000" "#ff6c6b" "#98be65" "#ECBE7B" "#0170bf" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(show-paren-mode t)
 '(sml/active-background-color "#98ece8")
 '(sml/active-foreground-color "#424242")
 '(sml/inactive-background-color "#4fa8a8")
 '(sml/inactive-foreground-color "#424242")
 '(tab-width 4)
 '(tool-bar-mode nil)
 '(vc-annotate-background "#000000")
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (list
    (cons 20 "#98be65")
    (cons 40 "#b4be6c")
    (cons 60 "#d0be73")
    (cons 80 "#ECBE7B")
    (cons 100 "#d9af76")
    (cons 120 "#c6a071")
    (cons 140 "#b4916d")
    (cons 160 "#ba8892")
    (cons 180 "#c080b7")
    (cons 200 "#c678dd")
    (cons 220 "#d974b7")
    (cons 240 "#ec7091")
    (cons 260 "#ff6c6b")
    (cons 280 "#d6696a")
    (cons 300 "#ad6769")
    (cons 320 "#836468")
    (cons 340 "#5B6268")
    (cons 360 "#5B6268")))
 '(vc-annotate-very-old-color nil)
 '(vc-follow-symlinks t)
 '(xterm-color-names
   ["black" "#ff8059" "#44bc44" "#eecc00" "#2fafff" "#feacd0" "#00d3d0" "gray65"])
 '(xterm-color-names-bright
   ["gray35" "#f4923b" "#70c900" "#cfdf30" "#79a8ff" "#f78fe7" "#4ae8fc" "white"]))

;;; needs to be after config

;;;(add-hook 'haskell-mode-hook 'lsp)
;;;(add-hook 'haskell-literate-mode-hook 'lsp)
;;;(add-hook 'rust-mode-hook 'lsp)
(setq haskell-process-type 'stack-ghci)

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

(set-face-foreground 'font-lock-comment-face "light grey")

;;; rust

(setq rust-format-on-save t)
(add-hook 'rust-mode-hook #'racer-mode)
(add-hook 'racer-mode-hook #'eldoc-mode)
;;(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)

;;; haskell setup

(require 'haskell-interactive-mode)
(require 'haskell-process)
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
(add-hook 'haskell-mode-hook 'flycheck-mode)
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
 '(default ((t (:family "Iosevka" :foundry "BE5N" :slant normal :weight normal :height 128 :width normal))))
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
 '(custom-enabled-themes '(doom-spacegrey))
 '(custom-safe-themes
   '("0685ffa6c9f1324721659a9cd5a8931f4bb64efae9ce43a3dba3801e9412b4d8" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "d5a878172795c45441efcd84b20a14f553e7e96366a163f742b95d65a3f55d71" "cae81b048b8bccb7308cdcb4a91e085b3c959401e74a0f125e7c5b173b916bf9" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "e3c64e88fec56f86b49dcdc5a831e96782baf14b09397d4057156b17062a8848" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "2c49d6ac8c0bf19648c9d2eabec9b246d46cb94d83713eaae4f26b49a8183fc4" "0a41da554c41c9169bdaba5745468608706c9046231bbbc0d155af1a12f32271" "e27556a94bd02099248b888555a6458d897e8a7919fd64278d1f1e8784448941" "ff3c57a5049010a76de8949ddb629d29e2ced42b06098e046def291989a4104a" "f4876796ef5ee9c82b125a096a590c9891cec31320569fc6ff602ff99ed73dca" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "0fe24de6d37ea5a7724c56f0bb01efcbb3fe999a6e461ec1392f3c3b105cc5ac" "4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab" "4f01c1df1d203787560a67c1b295423174fd49934deb5e6789abd1e61dba9552" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "c086fe46209696a2d01752c0216ed72fd6faeabaaaa40db9fc1518abebaf700d" "56d10d2b60685d112dd54f4ba68a173c102eacc2a6048d417998249085383da1" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "fd22c8c803f2dac71db953b93df6560b6b058cb931ac12f688def67f08c10640" "f94110b35f558e4c015b2c680f150bf8a19799d775f8352c957d9d1054b0a210" "75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" "ca70827910547eb99368db50ac94556bbd194b7e8311cfbdbdcad8da65e803be" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "fce3524887a0994f8b9b047aef9cc4cc017c5a93a5fb1f84d300391fba313743" "95d0ed21bb0e919be7687a25ad59a1c2c8df78cbe98c9e369d44e65bfd65b167" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "0e2a7e1e632dd38a8e0227d2227cb8849f877dd878afb8219cb6bcdd02068a52" "7546a14373f1f2da6896830e7a73674ef274b3da313f8a2c4a79842e8a93953e" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "d9646b131c4aa37f01f909fbdd5a9099389518eb68f25277ed19ba99adeb7279" "d9495c98266e15a77c3cd6cb45f7964891c189cf613337d9a2e2950881493c09" default))
 '(display-line-numbers-type 'relative t)
 '(exwm-floating-border-color "#646464")
 '(fci-rule-color "#5B6268")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(haskell-mode-hook '(flycheck-mode interactive-haskell-mode))
 '(haskell-mode-stylish-haskell-path "ormolu")
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-stylish-on-save t)
 '(haskell-w3m-haddock-dirs '("~/.ghcup/share/doc/"))
 '(highlight-tail-colors '(("#2f4a00" . 0) ("#00415e" . 20)))
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
 '(lsp-eldoc-enable-hover t)
 '(lsp-eldoc-render-all nil)
 '(lsp-enable-file-watchers nil)
 '(lsp-enable-indentation t)
 '(lsp-haskell-format-on-import-on t)
 '(lsp-haskell-formatting-provider "ormolu")
 '(lsp-signature-render-documentation nil)
 '(lsp-ui-doc-alignment 'window)
 '(lsp-ui-doc-enable t)
 '(lsp-ui-doc-use-childframe t)
 '(lsp-ui-peek-enable t)
 '(lsp-ui-peek-list-width 20)
 '(lsp-ui-sideline-show-code-actions nil)
 '(lsp-ui-sideline-show-symbol nil)
 '(objed-cursor-color "#ff6c6b")
 '(org-src-block-faces 'nil)
 '(package-selected-packages
   '(company lsp-ui spacemacs-theme doom-themes cyberpunk-theme lsp-haskell racer rust-mode rmsbolt peep-dired flycheck w3m exec-path-from-shell python-mode nix-mode racket-mode function-args haskell-mode helm-slime slime elpher fish-mode cider paredit clojure-mode helm lsp-mode magit zig-mode yaml-mode meson-mode))
 '(pdf-view-midnight-colors (cons "#bbc2cf" "#000000"))
 '(rustic-ansi-faces
   ["#000000" "#ff6c6b" "#98be65" "#ECBE7B" "#0170bf" "#c678dd" "#46D9FF" "#bbc2cf"])
 '(show-paren-mode t)
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

(add-hook 'haskell-mode-hook 'lsp)
;(add-hook 'haskell-literate-mode-hook 'lsp)
;(add-hook 'rust-mode-hook 'lsp)

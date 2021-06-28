;;; configure melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(setq my-packages
      '(
	;;exec-path-from-shell
        flycheck
        haskell-mode
        helm-slime
        slime
        elpher
        fish-mode
        cider
        paredit
        clojure-mode
        helm
        magit
        zig-mode
        yaml-mode
        meson-mode
        use-package
	    tree-sitter
	    tree-sitter-langs
	    lsp-mode
	    lsp-ui
        dart-mode
        lsp-dart
        evil
        evil-surround
        lua-mode
        lsp-haskell
        company
        company-ghci
        ; nix-mode
        )
      )

;; (when (not package-archive-contents)
;;   (package-refresh-contents))



(require 'cl-lib)
(package-initialize)
(unless (cl-every #'package-installed-p my-packages)
  (dolist (package my-packages)
    (unless (package-installed-p package)
      (package-install package))))

(setq erc-hide-list '("JOIN" "PART" "QUIT"))

;;(exec-path-from-shell-initialize)

(setq-default indent-tabs-mode nil)
;;; js org mode
(org-babel-do-load-languages
      'org-babel-load-languages
      '((js . t)))

;;; goddamn comments

;;; treesitter

(require 'tree-sitter)
(require 'tree-sitter-langs)
(global-tree-sitter-mode)
(add-hook 'tree-sitter-after-on-hook #'tree-sitter-hl-mode)


;;(set-face-foreground 'font-lock-comment-face "grey")

;; evil

;; (setq display-line-numbers-type 'relative)
;; (global-display-line-numbers-mode)

;; (use-package evil
;;   :ensure t
;;   )
;; (evil-mode 1)

;; (use-package evil-surround
;;   :ensure t
;;   :config
;;   (global-evil-surround-mode 1))

;;; python

(use-package python
  :init
  (setq-default indent-tabs-mode nil)

  :config
  (setq python-indent-offset 2))

;;; rust

(use-package rust-mode)

(use-package racer
  :ensure t
  :after rust-mode
  :diminish racer-mode
  :init
  (add-hook 'rust-mode-hook #'racer-mode)
  (add-hook 'racer-mode-hook (lambda () (setq eldoc-documentation-function nil))))

;;;(setq rust-format-on-save t)
;;;(add-hook 'rust-mode-hook #'racer-mode)
;;;(add-hook 'racer-mode-hook #'eldoc-mode)
;;(define-key rust-mode-map (kbd "C-c C-c") 'rust-run)

;;; haskell setup

(require 'haskell-interactive-mode)
(require 'haskell-process)

(use-package lsp-haskell
  :ensure t
  :config
  (setq lsp-log-io t)
)

;;; dart setup
(use-package dart-mode
  :ensure t
  :init
  (setq dart-format-on-save t)
  )
(add-hook 'dart-mode-hook (lambda () (lsp-mode +1)))

;;; lsp

(use-package lsp-mode
  :ensure t
  ;; :disabled t
  )

(use-package lsp-ui
  ;; :init (add-hook 'lsp-mode-hook 'lsp-ui-mode)
  :ensure t
  ;; :disabled t
  ;; :hook (lsp-mode . lsp-ui-mode)
  :hook (lsp-mode . lsp-ui-mode)
  )
;; doesn't work
;;(define-key lsp-mode-map (kbd "C-z C-z") lsp-command-map)

;; (defun user-haskell-save-hook ()
;;   (when (eq major-mode 'haskell-mode)
;;     (shell-command-to-string (format "brittany --write-mode inplace %s" buffer-file-name))
;;     (revert-buffer :ignore-auto :noconfirm)
;;     )
;;   ) 
;;(add-hook 'after-save-hook #'user-haskell-save-hook)
;;(setq haskell-mode-stylish-haskell-path "ormolu") 

;;;(require 'flycheck)
;;;(add-hook 'haskell-mode-hook 'flycheck-mode)
;;;;;(setq flycheck-display-errors-function nil)
;;; '(flycheck-check-syntax-automatically (quote ( mode-enabled saved)))
;;;;; '(flycheck-idle-change-delay 4) ;; Set delay based on what suits you the best
(use-package flycheck
  :init (global-flycheck-mode)
  :ensure t
  :disabled t)


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
(use-package paredit
  :defer t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'clojurescript-mode-hook 'paredit-mode)
    (add-hook 'clojurec-mode-hook 'paredit-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode)))
;;;(paredit-mode 1)
;;;(add-hook 'clojure-mode-hook #'paredit-mode)
;;;(show-paren-mode 1)
;;;(autoload 'enable-paredit-mode "paredit" "Turn on pseudo-structural editing of Lisp code." t)
;;;(add-hook 'emacs-lisp-mode-hook       #'enable-paredit-mode)
;;;(add-hook 'eval-expression-minibuffer-setup-hook #'enable-paredit-mode)
;;;(add-hook 'ielm-mode-hook             #'enable-paredit-mode)
;;;(add-hook 'lisp-mode-hook             #'enable-paredit-mode)
;;;(add-hook 'racket-mode-hook           #'enable-paredit-mode)
;;;(add-hook 'lisp-interaction-mode-hook #'enable-paredit-mode)
;;;(add-hook 'scheme-mode-hook           #'enable-paredit-mode)

;;; lua

(use-package lua-mode
  :ensure t
  :disabled t)
;;; clojure

(use-package cider
  :defer t
  :init
  (progn
    (add-hook 'clojure-mode-hook 'cider-mode)
    (add-hook 'clojurescript-mode-hook 'cider-mode)
    (add-hook 'clojurec-mode-hook 'cider-mode)
    (add-hook 'cider-repl-mode-hook 'cider-mode))
  :config
  (setq cider-repl-display-help-banner nil)
  (setq cider-auto-mode nil))

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
;;;(add-hook 'c-mode-hook 'lsp)
;;;(add-hook 'c++-mode-hook 'lsp)

(add-hook 'c++-mode-hook (lambda () (c-toggle-comment-style 1)))
;(setq lsp-signature-auto-activate t)

;;; stop indenting please
(setq lsp-enable-indentation nil)

;;; WHY DO I KEEP QUITTING
(setq confirm-kill-emacs 'yes-or-no-p)

;;; relative line numbers
;;;(setq display-line-numbers-type 'relative)

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
 '(default ((t (:family "Fantasque Sans Mono" :foundry "PfEd" :slant normal :weight normal :height 103 :width normal))))
 '(lsp-ui-doc-background ((t (:inherit tooltip))))
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
 '(ansi-term-color-vector
   [unspecified "#2d2a2e" "#ff6188" "#a9dc76" "#ffd866" "#78dce8" "#ab9df2" "#a1efe4" "#fcfcfa"])
 '(awesome-tray-mode-line-active-color "#2fafff")
 '(awesome-tray-mode-line-inactive-color "#323232")
 '(blink-cursor-mode nil)
 '(column-number-mode t)
 '(custom-enabled-themes '(monokai-pro))
 '(custom-safe-themes
   '("1263771faf6967879c3ab8b577c6c31020222ac6d3bac31f331a74275385a452" "6daa09c8c2c68de3ff1b83694115231faa7e650fdbb668bc76275f0f2ce2a437" "b0e446b48d03c5053af28908168262c3e5335dcad3317215d9fdeb8bac5bacf9" "1d5e33500bc9548f800f9e248b57d1b2a9ecde79cb40c0b1398dec51ee820daf" "4133d2d6553fe5af2ce3f24b7267af475b5e839069ba0e5c80416aa28913e89a" "6c386d159853b0ee6695b45e64f598ed45bd67c47f671f69100817d7db64724d" "e6f3a4a582ffb5de0471c9b640a5f0212ccf258a987ba421ae2659f1eaa39b09" "745d03d647c4b118f671c49214420639cb3af7152e81f132478ed1c649d4597d" "b9a67b48d56c580cb300ce9c1ecc3b83aee953346a33e2a14b31e2e4a07ea8a6" "e3a1b1fb50e3908e80514de38acbac74be2eb2777fc896e44b54ce44308e5330" "b6269b0356ed8d9ed55b0dcea10b4e13227b89fd2af4452eee19ac88297b0f99" "b02eae4d22362a941751f690032ea30c7c78d8ca8a1212fdae9eecad28a3587f" "da53441eb1a2a6c50217ee685a850c259e9974a8fa60e899d393040b4b8cc922" "5784d048e5a985627520beb8a101561b502a191b52fa401139f4dd20acb07607" "6b1abd26f3e38be1823bd151a96117b288062c6cde5253823539c6926c3bb178" "37768a79b479684b0756dec7c0fc7652082910c37d8863c35b702db3f16000f8" "c8b83e7692e77f3e2e46c08177b673da6e41b307805cd1982da9e2ea2e90e6d7" "24168c7e083ca0bbc87c68d3139ef39f072488703dcdd82343b8cab71c0f62a7" "fb83a50c80de36f23aea5919e50e1bccd565ca5bb646af95729dc8c5f926cbf3" "a3bdcbd7c991abd07e48ad32f71e6219d55694056c0c15b4144f370175273d16" "39fe48be738ea23b0295cdf17c99054bb439a7d830248d7e6493c2110bfed6f8" "7beac4a68f03662b083c9c2d4f1d7f8e4be2b3d4b0d904350a9edf3cf7ce3d7f" "b47eca77c785108ab443aea40fbabb2af3e13a3ac8a8537975dee099b866a0f0" "8ecc63db23c4b608a2ba60974c8fe7a0f8c57d8563f83c5c126f002d6ae2d48e" "e8a0c94af8c0eeec7ae0f1633d29098ea722e5765f1e9c67b49da6f3414b9bfe" "c3957b559cf3606c9a40777c5712671db3c7538e5d5ea9f63eb0729afeac832b" "9be1d34d961a40d94ef94d0d08a364c3d27201f3c98c9d38e36f10588469ea57" "93268bf5365f22c685550a3cbb8c687a1211e827edc76ce7be3c4bd764054bad" "5a7830712d709a4fc128a7998b7fa963f37e960fd2e8aa75c76f692b36e6cf3c" "96be1c5bb74fc2ffdfed87e46c87f1492969bf2af1fc96232e35c06b517aecc1" "8c1dd3d6fdfb2bee6b8f05d13d167f200befe1712d0abfdc47bb6d3b706c3434" "16dd114a84d0aeccc5ad6fd64752a11ea2e841e3853234f19dc02a7b91f5d661" "36282815a2eaab9ba67d7653cf23b1a4e230e4907c7f110eebf3cdf1445d8370" "c968804189e0fc963c641f5c9ad64bca431d41af2fb7e1d01a2a6666376f819c" "1f538e3de6aa8711e3ad8f297a57ee8210ade0f46f411c06343d126a4fea72c6" "bc4c89a7b91cfbd3e28b2a8e9e6750079a985237b960384f158515d32c7f0490" "b6493078dc17528c57a3eb6b334e496217d026d856261b349603c9845f3cf2d8" "1bb8f76bcd04a2b25a663a3da69235fbdbe9db1d5fe7efc6e8fcfc5e1030c9c3" "6145e62774a589c074a31a05dfa5efdf8789cf869104e905956f0cbd7eda9d0e" "3380a2766cf0590d50d6366c5a91e976bdc3c413df963a0ab9952314b4577299" "c560237b7505f67a271def31c706151afd7aa6eba9f69af77ec05bde5408dbcd" "cbd85ab34afb47003fa7f814a462c24affb1de81ebf172b78cb4e65186ba59d2" "d0fd069415ef23ccc21ccb0e54d93bdbb996a6cce48ffce7f810826bb243502c" "ffba0482d3548c9494e84c1324d527f73ea4e43fff8dfd0e48faa8fc6d5c2bc7" "8f5b54bf6a36fe1c138219960dd324aad8ab1f62f543bed73ef5ad60956e36ae" "23b564cfb74d784c73167d7de1b9a067bcca00719f81e46d09ee71a12ef7ee82" "df4a201edd54dbce57d029c3f1d91ccbf3a06ce22365c0d00d669442fcb2bced" "04a31c2a707e2ca1cb5c411a3b9e183760726c9d61983f58d95ce37c1c97d086" "ac6e2b8f5c58c4710f59b7d2652bab7b715476696b05f5395f6b5cdd64e41160" "18cd5a0173772cdaee5522b79c444acbc85f9a06055ec54bb91491173bc90aaa" "5d59bd44c5a875566348fa44ee01c98c1d72369dc531c1c5458b0864841f887c" "3df5335c36b40e417fec0392532c1b82b79114a05d5ade62cfe3de63a59bc5c6" "5379937b99998e0510bd37ae072c7f57e26da7a11e9fb7bced8b94ccc766c804" "7b3d184d2955990e4df1162aeff6bfb4e1c3e822368f0359e15e2974235d9fa8" "08a27c4cde8fcbb2869d71fdc9fa47ab7e4d31c27d40d59bf05729c4640ce834" "0685ffa6c9f1324721659a9cd5a8931f4bb64efae9ce43a3dba3801e9412b4d8" "1623aa627fecd5877246f48199b8e2856647c99c6acdab506173f9bb8b0a41ac" "54cf3f8314ce89c4d7e20ae52f7ff0739efb458f4326a2ca075bf34bc0b4f499" "d5a878172795c45441efcd84b20a14f553e7e96366a163f742b95d65a3f55d71" "cae81b048b8bccb7308cdcb4a91e085b3c959401e74a0f125e7c5b173b916bf9" "f2927d7d87e8207fa9a0a003c0f222d45c948845de162c885bf6ad2a255babfd" "e3c64e88fec56f86b49dcdc5a831e96782baf14b09397d4057156b17062a8848" "6c3b5f4391572c4176908bb30eddc1718344b8eaff50e162e36f271f6de015ca" "2c49d6ac8c0bf19648c9d2eabec9b246d46cb94d83713eaae4f26b49a8183fc4" "0a41da554c41c9169bdaba5745468608706c9046231bbbc0d155af1a12f32271" "e27556a94bd02099248b888555a6458d897e8a7919fd64278d1f1e8784448941" "ff3c57a5049010a76de8949ddb629d29e2ced42b06098e046def291989a4104a" "f4876796ef5ee9c82b125a096a590c9891cec31320569fc6ff602ff99ed73dca" "3c2f28c6ba2ad7373ea4c43f28fcf2eed14818ec9f0659b1c97d4e89c99e091e" "990e24b406787568c592db2b853aa65ecc2dcd08146c0d22293259d400174e37" "e074be1c799b509f52870ee596a5977b519f6d269455b84ed998666cf6fc802a" "fa2b58bb98b62c3b8cf3b6f02f058ef7827a8e497125de0254f56e373abee088" "7a994c16aa550678846e82edc8c9d6a7d39cc6564baaaacc305a3fdc0bd8725f" "93ed23c504b202cf96ee591138b0012c295338f38046a1f3c14522d4a64d7308" "5b809c3eae60da2af8a8cfba4e9e04b4d608cb49584cb5998f6e4a1c87c057c4" "0fe24de6d37ea5a7724c56f0bb01efcbb3fe999a6e461ec1392f3c3b105cc5ac" "4a8d4375d90a7051115db94ed40e9abb2c0766e80e228ecad60e06b3b397acab" "4f01c1df1d203787560a67c1b295423174fd49934deb5e6789abd1e61dba9552" "730a87ed3dc2bf318f3ea3626ce21fb054cd3a1471dcd59c81a4071df02cb601" "7d708f0168f54b90fc91692811263c995bebb9f68b8b7525d0e2200da9bc903c" "c086fe46209696a2d01752c0216ed72fd6faeabaaaa40db9fc1518abebaf700d" "56d10d2b60685d112dd54f4ba68a173c102eacc2a6048d417998249085383da1" "79278310dd6cacf2d2f491063c4ab8b129fee2a498e4c25912ddaa6c3c5b621e" "2cdc13ef8c76a22daa0f46370011f54e79bae00d5736340a5ddfe656a767fddf" "bffa9739ce0752a37d9b1eee78fc00ba159748f50dc328af4be661484848e476" "6b80b5b0762a814c62ce858e9d72745a05dd5fc66f821a1c5023b4f2a76bc910" "c83c095dd01cde64b631fb0fe5980587deec3834dc55144a6e78ff91ebc80b19" "fd22c8c803f2dac71db953b93df6560b6b058cb931ac12f688def67f08c10640" "f94110b35f558e4c015b2c680f150bf8a19799d775f8352c957d9d1054b0a210" "75b8719c741c6d7afa290e0bb394d809f0cc62045b93e1d66cd646907f8e6d43" "ca70827910547eb99368db50ac94556bbd194b7e8311cfbdbdcad8da65e803be" "01cf34eca93938925143f402c2e6141f03abb341f27d1c2dba3d50af9357ce70" "d74c5485d42ca4b7f3092e50db687600d0e16006d8fa335c69cf4f379dbd0eee" "fce3524887a0994f8b9b047aef9cc4cc017c5a93a5fb1f84d300391fba313743" "95d0ed21bb0e919be7687a25ad59a1c2c8df78cbe98c9e369d44e65bfd65b167" "e1ef2d5b8091f4953fe17b4ca3dd143d476c106e221d92ded38614266cea3c8b" "aaa4c36ce00e572784d424554dcc9641c82d1155370770e231e10c649b59a074" "8f5a7a9a3c510ef9cbb88e600c0b4c53cdcdb502cfe3eb50040b7e13c6f4e78e" "b5fff23b86b3fd2dd2cc86aa3b27ee91513adaefeaa75adc8af35a45ffb6c499" "c4bdbbd52c8e07112d1bfd00fee22bf0f25e727e95623ecb20c4fa098b74c1bd" "4bca89c1004e24981c840d3a32755bf859a6910c65b829d9441814000cf6c3d0" "e6ff132edb1bfa0645e2ba032c44ce94a3bd3c15e3929cdf6c049802cf059a2a" "be9645aaa8c11f76a10bcf36aaf83f54f4587ced1b9b679b55639c87404e2499" "0e2a7e1e632dd38a8e0227d2227cb8849f877dd878afb8219cb6bcdd02068a52" "7546a14373f1f2da6896830e7a73674ef274b3da313f8a2c4a79842e8a93953e" "76bfa9318742342233d8b0b42e824130b3a50dcc732866ff8e47366aed69de11" "b89a4f5916c29a235d0600ad5a0849b1c50fab16c2c518e1d98f0412367e7f97" "d9646b131c4aa37f01f909fbdd5a9099389518eb68f25277ed19ba99adeb7279" "d9495c98266e15a77c3cd6cb45f7964891c189cf613337d9a2e2950881493c09" default))
 '(exwm-floating-border-color "#646464")
 '(fci-rule-color "#5B6268")
 '(flymake-error-bitmap '(flymake-double-exclamation-mark modus-theme-fringe-red))
 '(flymake-note-bitmap '(exclamation-mark modus-theme-fringe-cyan))
 '(flymake-warning-bitmap '(exclamation-mark modus-theme-fringe-yellow))
 '(haskell-mode-hook '(interactive-haskell-mode))
 '(haskell-mode-stylish-haskell-path "ormolu")
 '(haskell-process-auto-import-loaded-modules t)
 '(haskell-process-log t)
 '(haskell-process-suggest-remove-import-lines t)
 '(haskell-stylish-on-save nil)
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
 '(line-number-mode nil)
 '(lsp-auto-guess-root t)
 '(lsp-dart-flutter-widget-guides nil)
 '(lsp-eldoc-enable-hover t)
 '(lsp-eldoc-render-all nil)
 '(lsp-enable-file-watchers nil)
 '(lsp-enable-indentation t)
 '(lsp-haskell-format-on-import-on t)
 '(lsp-haskell-formatting-provider "ormolu")
 '(lsp-ui-doc-alignment 'window)
 '(lsp-ui-doc-enable t)
 '(lsp-ui-peek-enable t)
 '(objed-cursor-color "#ff6c6b")
 '(org-src-block-faces 'nil)
 '(package-selected-packages
   '(company-ghci tree-sitter-indent tree-sitter-langs tree-sitter fzf monokai-pro-theme vscode-dark-plus-theme evil nord-theme csv-mode lsp-dart mood-one-theme nothing-theme phoenix-dark-mono-theme punpun-theme quasi-monochrome-theme spacegray-theme pkgbuild-mode dart-mode flutter almost-mono-themes sexy-monochrome-theme purp-theme prassee-theme plan9-theme naysayer-theme company lsp-ui spacemacs-theme cyberpunk-theme lsp-haskell racer rust-mode rmsbolt peep-dired flycheck w3m exec-path-from-shell python-mode nix-mode racket-mode function-args haskell-mode helm-slime slime elpher fish-mode cider paredit clojure-mode helm lsp-mode magit zig-mode yaml-mode meson-mode))
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

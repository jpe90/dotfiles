;; configure melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)

(when (not package-archive-contents)
  (package-refresh-contents))

(when (featurep 'native-compile)
  (setq native-comp-deferred-compilation t)
  (setq native-comp-async-report-warnings-errors nil))


(setq erc-hide-list '("JOIN" "PART" "QUIT"))
;; (setq erc-nick "jpe")
;; (erc-tls :server "irc.libera.chat" :port 6697 :nick "jpe")
;; (setq erc-prompt-for-nickserv-password t)
;; (setq lispy-compat '(edebug cider))

(xterm-mouse-mode 1)
(setq-default cursor-type 'box)

(setq custom-safe-themes t)
;; stop opening vertical splits
(setq split-height-threshold nil)

;; Set default font
;; (set-face-font 'default "Hack Nerd Font Mono:size=12")
;; (set-face-font 'default "Inconsolata")
;; (set-face-font 'default "Jetbrains Mono:size=12")
(set-face-attribute 'default nil :font "Terminus-10:regular")

(setq-default indent-tabs-mode nil)
(setq visible-cursor nil)
(setq nrepl-use-ssh-fallback-for-remote-hosts t)

;; opacity

;; ;; set transparency
;; (set-frame-parameter (selected-frame) 'alpha '(85 85))
;; (add-to-list 'default-frame-alist '(alpha 85 85))

;;; org mode code eval
(org-babel-do-load-languages
      'org-babel-load-languages
      '((js . t)
        (lisp . t)))

 (setq org-babel-lisp-eval-fn #'sly-eval)


;;; scroll like vim
(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")
(global-set-key (kbd "<next>") 'View-scroll-half-page-forward)
(global-set-key (kbd "<prior>") 'View-scroll-half-page-backward)

;;; toolbar visibility
(tool-bar-mode -1)
(toggle-scroll-bar -1)
(menu-bar-mode -1)

(setq-default tab-width 4)

;;; cycle thru marks w/ c-space
(setq set-mark-command-repeat-pop t)

;;; turn off mouse acceleration
(setq mouse-wheel-progressive-speed nil)

;;; case insenstive autocompletion
(setq read-file-name-completion-ignore-case t)

;;; hide show
(add-hook 'prog-mode-hook #'hs-minor-mode)
(global-set-key (kbd "C-c <right>") 'hs-show-block)
(global-set-key (kbd "C-c <left>") 'hs-hide-block)
(setq hs-hide-comments-when-hiding-all nil)

(use-package paren
  :config
  (show-paren-mode +1))

(use-package company
  :ensure t
  :hook
  (prog-mode . company-mode)
  (racket-repl-mode . company-mode)
  (sly-mode . company-mode))

(use-package evil
  :ensure t
  :disabled t
  )
(use-package evil-surround
  :ensure t
  :disabled t
  :config
  (global-evil-surround-mode 1))

(use-package composite
  :hook (prog-mode . auto-composition-mode)
  :init (global-auto-composition-mode -1))

;; Colorizes delimiters so they can be told apart
;; (use-package rainbow-delimiters
;;              :ensure t
;;              :config (add-hook 'prog-mode-hook 'rainbow-delimiters-mode))

;;; haskell setup

(use-package racket-mode
  :ensure t
  :hook (racket-mode . racket-xp-mode))

(require 'haskell-interactive-mode)
(require 'haskell-process)

(use-package dart-mode
  :ensure t
  :hook (dart-mode . lsp))

(use-package haskell-mode
  :ensure t
  :disabled t
  :config
  (define-key haskell-mode-map [f5] (lambda () (interactive) (compile "stack build --fast")))
  :hook (haskell-mode . lsp)
  )

(use-package lsp-mode
  :ensure t
  :disabled t
  :config
  (setq lsp-enable-indentation nil))

(use-package lsp-haskell
  :ensure t
  :after lsp-mode
  :disabled t
  :config
  (setq lsp-log-io t)
)

(use-package lsp-ui
  :ensure t
  :disabled ;; :hook (lsp-mode . lsp-ui-mode)
  :hook (lsp-mode . lsp-ui-mode)
  :config
  (setq lsp-headerline-breadcrumb-enable nil)
  )

(use-package flycheck
  :ensure t
  :disabled t
  )

(use-package paredit
  ;; :defer t
  :ensure t
  :init
  (progn
    (add-hook 'emacs-lisp-mode-hook 'paredit-mode)
    (add-hook 'clojure-mode-hook 'paredit-mode)
    (add-hook 'clojurescript-mode-hook 'paredit-mode)
    (add-hook 'clojurec-mode-hook 'paredit-mode)
    (add-hook 'cider-repl-mode-hook 'paredit-mode)
    (add-hook 'racket-mode-hook 'paredit-mode)
    (add-hook 'racket-repl-mode-hook 'paredit-mode)
    )
  (bind-key "C-M-w" 'select-and-copy-between-parens)
  :hook
  (sly-mode . paredit-mode)
  )

(use-package cider
  ;; :defer t
  :ensure t
  :bind (("C-c =" . cider-format-buffer))
  :init
  (progn
    (add-hook 'clojure-mode-hook 'cider-mode)
    (add-hook 'clojurescript-mode-hook 'cider-mode)
    (add-hook 'clojurec-mode-hook 'cider-mode)
    (add-hook 'cider-repl-mode-hook 'cider-mode))
  :config
  (setq cider-repl-display-help-banner nil)
  (setq cider-auto-mode nil)
  (setq show-paren-mode t)
  )

(use-package exec-path-from-shell
  :if (memq window-system '(mac ns))
  :ensure t
  :config
  (exec-path-from-shell-initialize))
;;; get rid of blinking cursor
(blink-cursor-mode 1)

(defun user-flymake-keybindings ()
  (local-set-key (kbd "M-p") 'flymake-goto-prev-error)
  (local-set-key (kbd "M-n") 'flymake-goto-next-error))
(add-hook 'flymake-mode-hook		#'user-flymake-keybindings)

;; lisp setup
(defun select-and-copy-between-parens ()
  (interactive)
  (mark-sexp)
  (copy-region-as-kill (region-beginning) (region-end)))

;;; gdb setup
(setq
 ;; use gdb-many-windows by default
 gdb-many-windows t
 ;; Non-nil means display source file containing the main routine at startup
 gdb-show-main t)

(use-package helm
  :ensure t
  :bind (("M-x" . helm-M-x)
         ("M-y" . helm-show-kill-ring)
         ("C-x C-f" . helm-find-files)
         ("C-x b" . helm-mini)
         ("C-c o" . helm-occur))
  :config  
  (helm-autoresize-mode t)
  (setq helm-autoresize-max-height 20)
  (setq helm-autoresize-min-height 20)
  (setq helm-split-window-in-side-p t))
(use-package helm-tramp
  :ensure t
  :after helm
  :init
  (setq helm-tramp-custom-connections '(/sshx:solaire@192.168.1.199:/home/solaire/development))
  :bind
  (:map global-map
        ("C-c s" . helm-tramp)))


(use-package sly
  :ensure t
  :init
  (setq inferior-lisp-program "sbcl")
  )

(add-hook 'c++-mode-hook (lambda () (c-toggle-comment-style 1)))

;;; stop indenting please


;;; WHY DO I KEEP QUITTING
(setq confirm-kill-emacs 'yes-or-no-p)

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
;; (setq path-to-ctags "/usr/bin/ctags")

;; (defun create-tags (dir-name)
;;   "Create tags file."
;;   (interactive "DDirectory: ")
;;   (shell-command (format "%s -f TAGS -e -R %s" path-to-ctags (directory-file-name dir-name)))
;;   )


;;; c indentation
(setq c-default-style "stroustrup")
(setq c-offsets-alist '((arglist-cont-nonempty . +)))

;;; config org mode
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)

;;; jump to header in c file
;; (add-hook 'c-mode-common-hook
;;   (lambda() 
;;     (local-set-key  (kbd "C-c o") 'ff-find-other-file)))

(setq column-number-mode t)
;; ########################## Custom

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:background nil))))
 '(lsp-ui-doc-background ((t (:inherit tooltip))))
 '(lsp-ui-sideline-symbol-info ((t (:extend t)))))
'(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
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
 '(helm-completion-style 'helm)
 '(inhibit-startup-screen t)
 '(line-number-mode t)
 '(linum-format " %5i ")
 '(lsp-auto-guess-root t)
 '(lsp-dart-flutter-widget-guides nil)
 '(lsp-eldoc-enable-hover t)
 '(lsp-eldoc-render-all nil)
 '(lsp-enable-file-watchers nil)
 '(lsp-enable-indentation t t)
 '(lsp-haskell-format-on-import-on t)
 '(lsp-haskell-formatting-provider "ormolu")
 '(lsp-haskell-hlint-on nil)
 '(lsp-ui-doc-border "#586e75")
 '(lsp-ui-doc-enable t)
 '(lsp-ui-peek-enable t)
 '(menu-bar-mode nil)
 '(org-src-block-faces 'nil)
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
 '(ansi-color-names-vector
   ["#212526" "#ff4b4b" "#b4fa70" "#fce94f" "#729fcf" "#e090d7" "#8cc4ff" "#eeeeec"])
 '(column-number-mode t)
 '(compilation-message-face 'default)
 '(custom-enabled-themes '(gruvbox-dark-hard))
 '(exwm-floating-border-color "#383838")
 '(fci-rule-color "#585659")
 '(highlight-tail-colors ((("#2a342c") . 0) (("#273335") . 20)))
 '(jdee-db-active-breakpoint-face-colors (cons "#131313" "#fce566"))
 '(jdee-db-requested-breakpoint-face-colors (cons "#131313" "#7bd88f"))
 '(jdee-db-spec-breakpoint-face-colors (cons "#131313" "#525053"))
 '(linum-format " %7i ")
 '(lsp-ui-doc-border "#93a1a1")
 '(lsp-ui-imenu-colors '("#7FC1CA" "#A8CE93"))
 '(magit-diff-use-overlays nil)
 '(menu-bar-mode nil)
 '(objed-cursor-color "#fc618d")
 '(package-selected-packages
   '(doom-themes su sly tango-plus-theme lsp-mode rainbow-delimiters gotham-theme nimbus-theme mood-one-theme night-owl-theme zig-mode yaml-mode use-package sublime-themes racket-mode project paredit naysayer-theme monokai-pro-theme meson-mode markdown-preview-mode magit lua-mode lsp-ui lsp-haskell lsp-dart lispy jetbrains-darcula-theme helm-tramp helm-rg hasklig-mode gruvbox-theme flycheck fish-mode evil-surround elpher dracula-theme company-ghci cider almost-mono-themes))
 '(rustic-ansi-faces
   ["#222222" "#fc618d" "#7bd88f" "#fce566" "#5ad4e6" "#5ad4e6" "#5ad4e6" "#f7f1ff"])
 '(show-paren-mode t)
 '(tool-bar-mode nil)
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   (list
    (cons 20 "#7bd88f")
    (cons 40 "#a6dc81")
    (cons 60 "#d1e073")
    (cons 80 "#fce566")
    (cons 100 "#fcc95f")
    (cons 120 "#fcae59")
    (cons 140 "#fd9353")
    (cons 160 "#c6a884")
    (cons 180 "#90beb5")
    (cons 200 "#5ad4e6")
    (cons 220 "#90adc8")
    (cons 240 "#c687aa")
    (cons 260 "#fc618d")
    (cons 280 "#d15c7e")
    (cons 300 "#a75870")
    (cons 320 "#7c5461")
    (cons 340 "#585659")
    (cons 360 "#585659")))
 '(vc-annotate-very-old-color nil)
 '(window-divider-mode nil))

(tool-bar-mode -1)
(setq set-mark-command-repeat-pop t)
(setq mouse-wheel-progressive-speed nil)
(toggle-scroll-bar -1)
(menu-bar-mode -1)
(setq c-default-style "stroustrup")
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
;;(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;;(setq indent-line-function 'insert-tab)
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

;; ########################## DO NOT MODIFY
(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "CYRE" :slant normal :weight normal :height 139 :width normal)))))
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(ansi-color-names-vector
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#839496"])
 '(compilation-message-face 'default)
 '(cua-global-mark-cursor-color "#2aa198")
 '(cua-normal-cursor-color "#657b83")
 '(cua-overwrite-cursor-color "#b58900")
 '(cua-read-only-cursor-color "#859900")
 '(custom-enabled-themes '(naysayer))
 '(custom-safe-themes
   '("13a8eaddb003fd0d561096e11e1a91b029d3c9d64554f8e897b2513dbf14b277" "2809bcb77ad21312897b541134981282dc455ccd7c14d74cc333b6e549b824f3" "5d59bd44c5a875566348fa44ee01c98c1d72369dc531c1c5458b0864841f887c" default))
 '(fci-rule-color "#eee8d5")
 '(highlight-changes-colors '("#d33682" "#6c71c4"))
 '(highlight-symbol-colors
   '("#ef17e073a647" "#cfb2ec5dc313" "#fd86d7d6a41f" "#dde3d57ed9d5" "#e2f8e06fa59f" "#fd86d7d6a41f" "#d49ce01ad933"))
 '(highlight-symbol-foreground-color "#586e75")
 '(highlight-tail-colors
   '(("#eee8d5" . 0)
	 ("#b3c34d" . 20)
	 ("#6ccec0" . 30)
	 ("#74adf5" . 50)
	 ("#e1af4b" . 60)
	 ("#fb7640" . 70)
	 ("#ff699e" . 85)
	 ("#eee8d5" . 100)))
 '(hl-bg-colors
   '("#e1af4b" "#fb7640" "#ff6849" "#ff699e" "#8d85e7" "#74adf5" "#6ccec0" "#b3c34d"))
 '(hl-fg-colors
   '("#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3" "#fdf6e3"))
 '(hl-paren-colors '("#2aa198" "#b58900" "#268bd2" "#6c71c4" "#859900"))
 '(inhibit-startup-screen t)
 '(linum-format " %5i ")
 '(lsp-ui-doc-border "#586e75")
 '(nrepl-message-colors
   '("#dc322f" "#cb4b16" "#b58900" "#5b7300" "#b3c34d" "#0061a8" "#2aa198" "#d33682" "#6c71c4"))
 '(package-selected-packages
   '(evil naysayer-theme zig-mode solarized-theme meson-mode fzf))
 '(pos-tip-background-color "#eee8d5")
 '(pos-tip-foreground-color "#586e75")
 '(smartrep-mode-line-active-bg (solarized-color-blend "#859900" "#eee8d5" 0.2))
 '(term-default-bg-color "#fdf6e3")
 '(term-default-fg-color "#657b83")
 '(vc-annotate-background nil)
 '(vc-annotate-background-mode nil)
 '(vc-annotate-color-map
   '((20 . "#dc322f")
	 (40 . "#c4c075980000")
	 (60 . "#ba837ef50000")
	 (80 . "#b58900")
	 (100 . "#a28d87c90000")
	 (120 . "#9be887d10000")
	 (140 . "#953287c80000")
	 (160 . "#8e6887af0000")
	 (180 . "#859900")
	 (200 . "#76f495013bd5")
	 (220 . "#6b039bb14fa2")
	 (240 . "#5adca25c6284")
	 (260 . "#4293a905750f")
	 (280 . "#2aa198")
	 (300 . "#247b9fbfa83b")
	 (320 . "#24bd97b8b831")
	 (340 . "#1d0e8fa5c80a")
	 (360 . "#268bd2")))
 '(vc-annotate-very-old-color nil)
 '(weechat-color-list
   '(unspecified "#fdf6e3" "#eee8d5" "#a7020a" "#dc322f" "#5b7300" "#859900" "#866300" "#b58900" "#0061a8" "#268bd2" "#a00559" "#d33682" "#007d76" "#2aa198" "#657b83" "#839496"))
 '(xterm-color-names
   ["#eee8d5" "#dc322f" "#859900" "#b58900" "#268bd2" "#d33682" "#2aa198" "#073642"])
 '(xterm-color-names-bright
   ["#fdf6e3" "#cb4b16" "#93a1a1" "#839496" "#657b83" "#6c71c4" "#586e75" "#002b36"]))

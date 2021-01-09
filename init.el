;;; scroll like vim
(autoload 'View-scroll-half-page-forward "view")
(autoload 'View-scroll-half-page-backward "view")
(global-set-key (kbd "C-v") 'View-scroll-half-page-forward)
(global-set-key (kbd "M-v") 'View-scroll-half-page-backward)

;;; backup/autosave
(defvar backup-dir (expand-file-name "~/.emacs.d/backup/"))
(defvar autosave-dir (expand-file-name "~/.emacs.d/autosave/"))
(setq backup-directory-alist (list (cons ".*" backup-dir)))
(setq auto-save-list-file-prefix autosave-dir)
(setq auto-save-file-name-transforms `((".*" ,autosave-dir t)))

;;; ctags config
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

;;; i forget
(setq tab-always-indent 'complete)

;;; cycle thru marks w/ c-space
(setq set-mark-command-repeat-pop t)

;;; turn off mouse acceleration
(setq mouse-wheel-progressive-speed nil)

;;; case insenstive autocompletion
(setq read-file-name-completion-ignore-case t)

;;; config org mode
(setq c-default-style "stroustrup")
(require 'org)
(define-key global-map "\C-cl" 'org-store-link)
(define-key global-map "\C-ca" 'org-agenda)
(setq org-log-done t)
;;(setq-default indent-tabs-mode nil)
(setq-default tab-width 4)
;;(setq indent-line-function 'insert-tab)

;;; configure melpa
(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)


;; ########################## DO NOT MODIFY
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(custom-safe-themes
   '("d9495c98266e15a77c3cd6cb45f7964891c189cf613337d9a2e2950881493c09" default)))
 '(package-selected-packages
   '(color-theme-sanityinc-solarized gruvbox-theme color-theme-sanityinc-tomorrow zenburn-theme dracula-theme monokai-theme afternoon-theme cyberpunk-theme yaml-mode oceanic-theme evil naysayer-theme zig-mode solarized-theme meson-mode fzf))

;; ################################### OK TO MODIFY

;;; theme when in GUI mode but not terminal mode
;;(if (display-graphic-p)
;;    (load-theme 'oceanic' t)) 

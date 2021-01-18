;;; helm setup
(require 'helm-config)
(helm-mode 1)
(global-set-key (kbd "C-x b") 'helm-buffers-list)
(global-set-key (kbd "C-s") 'helm-occur)
(global-set-key (kbd "M-y") 'helm-show-kill-ring)

;;;; lsp setup
;;; prefix for LSP commands
(setq lsp-keymap-prefix "C-c C-l")

(require 'lsp-mode)
(add-hook 'c-mode-hook 'lsp)

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

;;; #DEPENDENCY universal ctags
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
(package-initialize)

;;; DEPENDENCY: fzf
;;; keybind for fzf
(global-set-key (kbd "M-p") 'fzf-git)

;; ########################## DO NOT MODIFY
(custom-set-variables
 ;; custom-set-variables was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(blink-cursor-mode nil)
 '(custom-safe-themes
   '("d9495c98266e15a77c3cd6cb45f7964891c189cf613337d9a2e2950881493c09" default))
 '(display-line-numbers-type 'relative t)
 '(package-selected-packages
   '(helm lsp-mode magit zig-mode yaml-mode meson-mode fzf evil))
 '(tool-bar-mode nil)
 '(vc-follow-symlinks t))
; '(package-selected-packages
 ;  '(yaml-mode evil zig-mode meson-mode fzf))

(custom-set-faces
 ;; custom-set-faces was added by Custom.
 ;; If you edit it by hand, you could mess it up, so be careful.
 ;; Your init file should contain only one such instance.
 ;; If there is more than one, they won't work right.
 '(default ((t (:family "Inconsolata" :foundry "CYRE" :slant normal :weight normal :height 120 :width normal)))))

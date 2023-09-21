(require 'package)
(add-to-list 'package-archives '("melpa" . "https://melpa.org/packages/") t)
(package-initialize)

(use-package better-defaults
  :ensure t)

(use-package sly
  :disabled t
  :ensure t
  :init
  (setq sly-net-coding-system 'utf-8-unix)
  (setq sly-lisp-implementations
        '((sbcl ("sbcl") :coding-system utf-8-unix)
          (ecl ("ecl") :coding-system utf-8-unix))))

(use-package xref
  :init
  ;; Use faster search tool
  (setq xref-search-program (cond
                             ((executable-find "ugrep") 'ugrep)
                             ((executable-find "rg") 'ripgrep)
                             (t 'grep)))

  ;; Select from xref candidates in minibuffer
  ;(setq xref-show-definitions-function #'xref-show-definitions-completing-read
  ;xref-show-xrefs-function #'xref-show-definitions-completing-read)
  )

(use-package magit
  :ensure t)

(defun previous-buffer ()
  "Switch to previously open buffer.
Repeated  toggle between the two most recently open buffers."
  (interactive)
  (switch-to-buffer (other-buffer (current-buffer) 1)))

(defun meow-setup ()
  (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
  (meow-motion-overwrite-define-key
   '("j" . meow-next)
   '("k" . meow-prev)
   '("<escape>" . ignore))
  (meow-leader-define-key
   ;; SPC j/k will run the original command in MOTION state.
   '("j" . "H-j")
   '("k" . "H-k")
   '("," . xref-pop-marker-stack)
   '("." . xref-find-definitions)
   '("f" . project-find-file)
   '("w" . other-window)
   '("W" . window-swap-states)
   '("o" . delete-other-windows)
   '("s" . split-window-right)
   '("-" . split-window-below)
   '("&" . +change-theme)
   '(";" . comment-line)
   '("K" . kill-this-buffer)
   '("d" . dired)
   '("b" . switch-to-buffer)
   '("r" . rg-project)
   '("f" . find-file)
   '("i" . imenu)
   '("a" . execute-extended-command)
   '("=" . smerge-basic-map)
   '("p" . project-find-file)
   '("j" . project-switch-to-buffer)
   '("t" . tab-bar-switch-to-tab)
   '("l" . project-switch-project)	
   '("y" . magit)
   '("n" . org-roam-keymap)
   '(":" . eval-expression)
   '("e" . "C-x C-e")
   ;; Use SPC (0-9) for digit arguments.
   '("1" . meow-digit-argument)
   '("2" . meow-digit-argument)
   '("3" . meow-digit-argument)
   '("4" . meow-digit-argument)
   '("5" . meow-digit-argument)
   '("6" . meow-digit-argument)
   '("7" . meow-digit-argument)
   '("8" . meow-digit-argument)
   '("9" . meow-digit-argument)
   '("0" . meow-digit-argument)
   '("/" . meow-keypad-describe-key)
   '("?" . meow-cheatsheet))
  (meow-normal-define-key
   '("0" . meow-expand-0)
   '("9" . meow-expand-9)
   '("8" . meow-expand-8)
   '("7" . meow-expand-7)
   '("6" . meow-expand-6)
   '("5" . meow-expand-5)
   '("4" . meow-expand-4)
   '("3" . meow-expand-3)
   '("2" . meow-expand-2)
   '("1" . meow-expand-1)
   '("-" . negative-argument)
   '(";" . meow-reverse)
   '("," . meow-inner-of-thing)
   '("." . meow-bounds-of-thing)
   '("<" . meow-beginning-of-thing)
   '(">" . meow-end-of-thing)
   '("a" . meow-append)
   '("A" . meow-open-below)
   '("b" . meow-back-word)
   '("B" . meow-back-symbol)
   '("c" . meow-change)
   '("d" . meow-delete)
   '("D" . meow-backward-delete)
   '("e" . meow-next-word)
   '("E" . meow-next-symbol)
   '("f" . meow-find)
   '("g" . meow-cancel-selection)
   '("G" . meow-grab)
   '("h" . meow-left)
   '("H" . meow-left-expand)
   '("i" . meow-insert)
   '("I" . meow-open-above)
   '("n" . meow-next)
   '("N" . meow-next-expand)
   '("p" . meow-prev)
   '("P" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("s" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("y" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("k" . meow-kill)
   '("t" . meow-till)
   '("u" . meow-undo)
   '("U" . meow-undo-in-selection)
   '("v" . meow-visit)
   '("w" . meow-mark-word)
   '("W" . meow-mark-symbol)
   '("x" . meow-line)
   '("X" . meow-goto-line)
   '("y" . meow-save)
   '("Y" . meow-sync-grab)
   '("z" . meow-pop-selection)
   '("'" . repeat)
   '("&" . meow-query-replace-regexp)
   '("%" . meow-query-replace)
   '("]" . scroll-up-command)
   '("[" . scroll-down-command)
   '("<escape>" . ignore)))

(use-package meow
  :ensure t
  :config
  (meow-setup)
  (meow-global-mode 1))

(use-package xclip
  :ensure t)

(setq
 mac-option-modifier 'meta
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
 tags-revert-without-query t
 edebug-print-length nil
 edebug-print-level nil
 use-short-answers t
 mac-pass-command-to-system nil
 mac-pass-control-to-system nil
 )
;; enable modes
(delete-selection-mode 1)
;;(pixel-scroll-mode 1)
(xclip-mode 1)
(xterm-mouse-mode 1)

(define-key global-map (kbd "C-z") nil)

(define-key global-map (kbd "C-<next>") nil)
(define-key global-map "\eo" #'previous-buffer)
(define-key global-map (kbd "C-M-o") #'delete-other-windows)
(define-key global-map (kbd "C-c b") #'switch-to-buffer)
;(define-key global-map (kbd "C-;") #'comment-line)
;(define-key global-map [f2] nil)
;(define-key global-map (kbd "C-,") 'project-find-regexp)
;(define-key global-map (kbd "C-\\") 'project-switch-to-buffer)
;;(define-key global-map (kbd "C-x b") 'switch-to-buffer)
;; (define-key global-map "\e3" 'exchange-point-and-mark)
(define-key global-map "\e`" #'other-frame)
(define-key global-map (kbd "<C-left>") #'back-to-indentation)
(define-key global-map (kbd "<C-right>") #'move-end-of-line)
(define-key global-map "\en" 'forward-paragraph)
(define-key global-map "\ep" 'backward-paragraph)
(define-key global-map [S-tab] 'indent-for-tab-command)
(define-key global-map (kbd "C-c p") 'project-find-file)
(setq-default indent-tabs-mode t)
(load "/Users/jon/.emacs.d/custom.el")
(load-theme 'standard-dark)

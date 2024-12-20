;; === meow ===

;; I really like Meow but I'm keeping this disabled. I wish the cursor
;; were a single character selection like in Kakoune. Needing to start
;; a selection with `w` before extending does not spark joy.

(use-package meow
  :ensure t)

;; (defun meow-setup ()
;;   (setq meow-cheatsheet-layout meow-cheatsheet-layout-qwerty)
;;   (meow-leader-define-key
;;    '("," . xref-pop-marker-stack)
;;    '("." . xref-find-definitions)
;;    '("f" . project-find-file)
;;    '("w" . other-window)
;;    '("W" . window-swap-states)
;;    '("o" . delete-other-windows)
;;    '("s" . split-window-right)
;;    '("v" . split-window-below)
;;    '("&" . +change-theme)
;;    '(";" . comment-line)
;;    '("K" . kill-this-buffer)
;;    '("d" . dired)
;;    '("b" . switch-to-buffer)
;;    '("r" . rg-project)
;;    '("f" . find-file)
;;    '("i" . imenu)
;;    '("a" . execute-extended-command)
;;    '("=" . org-store-link)
;;    '("p" . project-find-file)
;;    '("j" . project-switch-to-buffer)
;;    '("t" . tab-bar-switch-to-tab)
;;    '("l" . project-switch-project)
;;    '("y" . magit)
;;    '("l" . recentf-open-files)
;;    '("n" . org-roam-keymap)
;;    '(":" . eval-expression)
;;    '("e" . "C-x C-e")
;;    '("u" . undo-tree-visualize)
;;    ;; Use SPC (0-9) for digit arguments.
;;    '("1" . meow-digit-argument)
;;    '("2" . meow-digit-argument)
;;    '("3" . meow-digit-argument)
;;    '("4" . meow-digit-argument)
;;    '("5" . meow-digit-argument)
;;    '("6" . meow-digit-argument)
;;    '("7" . meow-digit-argument)
;;    '("8" . meow-digit-argument)
;;    '("9" . meow-digit-argument)
;;    '("0" . meow-digit-argument)
;;    '("/" . meow-keypad-describe-key)
;;    '("?" . meow-cheatsheet)
;;    '("@" . mark-sexp))
;;   (meow-normal-define-key
;;    '("0" . meow-expand-0)
;;    '("9" . meow-expand-9)
;;    '("8" . meow-expand-8)
;;    '("7" . meow-expand-7)
;;    '("6" . meow-expand-6)
;;    '("5" . meow-expand-5)
;;    '("4" . meow-expand-4)
;;    '("3" . meow-expand-3)
;;    '("2" . meow-expand-2)
;;    '("1" . meow-expand-1)
;;    '("-" . negative-argument)
;;    '(";" . meow-reverse)
;;    '("," . meow-inner-of-thing)
;;    '("." . meow-bounds-of-thing)
;;    '("<" . meow-beginning-of-thing)
;;    '(">" . meow-end-of-thing)
;;    '("a" . meow-append)
;;    '("A" . meow-open-below)
;;    '("b" . meow-back-word)
;;    '("B" . meow-back-symbol)
;;    '("c" . meow-change)
;;    '("d" . meow-delete)
;;    '("D" . meow-backward-delete)
;;    '("e" . meow-next-word)
;;    '("E" . meow-next-symbol)
;;    '("f" . meow-find)
;;    '("g" . meow-cancel-selection)
;;    '("G" . meow-grab)
;;    '("h" . meow-left)
;;    '("H" . meow-left-expand)
;;    '("i" . meow-insert)
;;    '("I" . meow-open-above)
;;    '("n" . meow-next)
;;    '("N" . meow-next-expand)
;;    '("p" . meow-prev)
;;    '("P" . meow-prev-expand)
;;    '("l" . meow-right)
;;    '("L" . meow-right-expand)
;;    '("m" . meow-join)
;;    '("s" . meow-search)
;;    '("o" . meow-block)
;;    '("O" . meow-to-block)
;;    '("j" . meow-yank)
;;    '("q" . meow-quit)
;;    '("Q" . meow-goto-line)
;;    '("r" . meow-replace)
;;    '("R" . meow-swap-grab)
;;    '("k" . meow-kill)
;;    '("t" . meow-till)
;;    '("u" . meow-undo)
;;    '("U" . meow-undo-in-selection)
;;    '("v" . meow-visit)
;;    '("w" . meow-mark-word)
;;    '("W" . meow-mark-symbol)
;;    '("x" . meow-line)
;;    '("X" . meow-goto-line)
;;    '("y" . meow-save)
;;    '("Y" . meow-sync-grab)
;;    '("z" . meow-pop-selection)
;;    '("'" . repeat)
;;    '("&" . meow-query-replace-regexp)
;;    '("%" . meow-query-replace)
;;    '("]" . scroll-up-command)
;;    '("[" . scroll-down-command)
;;    '("{" . backward-paragraph)
;;    '("}" . forward-paragraph)
;;    '("<escape>" . ignore)))

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
   '("[" . meow-beginning-of-thing)
   '("]" . meow-end-of-thing)
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
   '("j" . meow-next)
   '("J" . meow-next-expand)
   '("k" . meow-prev)
   '("K" . meow-prev-expand)
   '("l" . meow-right)
   '("L" . meow-right-expand)
   '("m" . meow-join)
   '("n" . meow-search)
   '("o" . meow-block)
   '("O" . meow-to-block)
   '("p" . meow-yank)
   '("q" . meow-quit)
   '("Q" . meow-goto-line)
   '("r" . meow-replace)
   '("R" . meow-swap-grab)
   '("s" . meow-kill)
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
   '("<escape>" . ignore)
   '("а" . meow-append)
   '("А" . meow-open-below)
   '("б" . meow-back-word)
   '("Б" . meow-back-symbol)
   '("ц" . meow-change)
   '("д" . meow-delete)
   '("Д" . meow-backward-delete)
   '("е" . meow-next-word)
   '("Е" . meow-next-symbol)
   '("ф" . meow-find)
   '("г" . meow-cancel-selection)
   '("Г" . meow-grab)
   '("ч" . meow-left)
   '("Ч" . meow-left-expand)
   '("и" . meow-insert)
   '("И" . meow-open-above)
   '("й" . meow-next)
   '("Й" . meow-next-expand)
   '("к" . meow-prev)
   '("К" . meow-prev-expand)
   '("л" . meow-right)
   '("Л" . meow-right-expand)
   '("м" . meow-join)
   '("н" . meow-search)
   '("о" . meow-block)
   '("О" . meow-to-block)
   '("п" . meow-yank)
   '("я" . meow-quit)
   '("Я" . meow-goto-line)
   '("р" . meow-replace)
   '("Р" . meow-swap-grab)
   '("с" . meow-kill)
   '("т" . meow-till)
   '("у" . meow-undo)
   '("У" . meow-undo-in-selection)
   '("в" . meow-visit)
   '("ш" . meow-mark-word)
   '("Ш" . meow-mark-symbol)
   '("х" . meow-line)
   '("Х" . meow-goto-line)
   '("ы" . meow-save)
   '("Ы" . meow-sync-grab)
   '("з" . meow-pop-selection)
   '("" . repeat)
   ))

(use-package meow
  :ensure t
  :config
  (meow-setup)
  (meow-global-mode 1)
  (setq meow-use-clipboard t)
  (setq meow-visit-sanitize-completion nil)
  )

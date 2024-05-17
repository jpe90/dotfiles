(deftheme witness "A custom Emacs theme.")

(let (
      (witness-default-fg "#d3b58d")
      (witness-default-bg "#072626")
      (witness-cursor-bg "lightgreen")
      (witness-custom-tag-fg "lightblue") ;
      (witness-green "#44b340")
      (witness-function-name-fg "white")
      (witness-keyword-fg "white")
      (witness-sea-green "#0fdfaf")
      (witness-variable-name-fg "#c8d4ec")
      (witness-warning-fg "#ffaa00")
      (witness-highlight-fg "navyblue")
      (witness-highlight-bg "darkseagreen2")
      (witness-region-bg "#0e3838")
      (witness-widget-field-fg "white")
      (witness-widget-field-bg "darkgray")
      (witness-minibuffer-prompt-fg "white")
      (witness-red       "#f43841")
      (witness-light-red "#ff4f58"))

  (custom-theme-set-faces
   'witness
   ;; fringe
   ;; `(mode-line ((t ,(list :background witness-default-fg
                          ;; :foreground witness-default-bg))))
   ;; `(mode-line-buffer-id ((t ,(list :background witness-default-fg
                                    ;; :foreground "white"))))
   ;; `(mode-line-inactive ((t ,(list :background witness-default-bg
                                   ;; :foreground witness-default-fg))))
   
   `(fringe ((t (:background ,witness-default-bg))))
   `(default ((t (:foreground ,witness-default-fg :background ,witness-default-bg))))
   `(cursor ((t (:background ,witness-cursor-bg))))
   `(custom-group-tag-face ((t (:underline t :foreground ,witness-custom-tag-fg))) t)
   `(custom-variable-tag-face ((t (:underline t :foreground ,witness-custom-tag-fg))) t)
   `(font-lock-builtin-face ((t nil)))
   `(font-lock-comment-face ((t (:foreground ,witness-green))))
   `(font-lock-function-name-face ((((class color) (background dark)) (:foreground ,witness-function-name-fg))))
   `(font-lock-keyword-face ((t (:foreground ,witness-keyword-fg))))
   `(font-lock-string-face ((t (:foreground ,witness-sea-green))))
   `(font-lock-variable-name-face ((((class color) (background dark)) (:foreground ,witness-variable-name-fg))))
   `(font-lock-warning-face ((t (:foreground ,witness-warning-fg))))
   `(highlight ((t (:foreground ,witness-highlight-fg :background ,witness-highlight-bg))))
   `(minibuffer-prompt ((t (:foreground ,witness-minibuffer-prompt-fg))))
   `(mode-line ((t (:inverse-video t))))
   `(region ((t (:background ,witness-region-bg)))) ;; Updated region face
   `(widget-field-face ((t (:foreground ,witness-widget-field-fg))) t)
   `(widget-single-line-field-face ((t (:background ,witness-widget-field-bg))) t)
   `(vertico-current ((t (:background ,witness-region-bg))))
   `(compilation-info ((t ,(list :foreground witness-green
                                 :inherit 'unspecified))))
   `(compilation-warning ((t ,(list :foreground witness-warning-fg
                                    :bold t
                                    :inherit 'unspecified))))
   `(compilation-error ((t (:foreground ,witness-light-red))))
   `(compilation-mode-line-fail ((t ,(list :foreground witness-red
                                           :weight 'bold
                                           :inherit 'unspecified))))
   `(compilation-mode-line-exit ((t ,(list :foreground witness-green
                                           :weight 'bold
                                           :inherit 'unspecified))))
;; (set-face-attribute 'completions-common-part nil
;;                     :foreground "yellow"
;;                     :bold t)
   `(completions-common-part ((t ,(list :foreground witness-highlight-bg
                                        :underline t
                                        :inherit 'unspecified))))
   ;; `(compilation-warning ((t (:background "orange" :inverse-video t))))
   ;; `(compilation-info ((t (:background "yellow" :inverse-video t))))

     ))

;;;###autoload
(when load-file-name
  (add-to-list 'custom-theme-load-path
               (file-name-as-directory (file-name-directory load-file-name))))


(provide-theme 'witness)

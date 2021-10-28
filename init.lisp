(in-package :stumpwm)
(ql:quickload :slynk)

(slynk:create-server :port 4004 :dont-close t)
;; (define-key *root-map* (kbd "[") "exchange-direction left")
(defparameter *terminal* "alacritty" "Stores the terminal emulator")
(defparameter *web-browser* "nyxt" "Stores the web browser")
(run-shell-command "xmodmap -e 'clear mod4'" t)
(run-shell-command "xmodmap -e \'keycode 133 = F20\'" t)
(set-prefix-key (kbd "F20"))
(define-key *root-map* (kbd "W") (concatenate 'string "exec " *web-browser*)) 
(define-key *root-map* (kbd "c") (concatenate 'string "exec " *terminal*))
(if (not (head-mode-line (current-head)))
    (toggle-mode-line (current-screen) (current-head)))
(setf *window-format* "%m%n%s%c")
(setf *screen-mode-line-format* (list "[^B%n^b] %W^>%d"))

(set-font "-xos4-terminus-medium-r-normal--12*")
(run-commands "mode-line" "mode-line") 

 ;;; Volume control

(defun current-volume-settings ()
  "Return current volume settings as multiple values (`MUTEDP', `VOLUME-LEFT-%', `VOLUME-RIGHT-%')."
  (let* ((raw-output (run-shell-command "pactl list sinks" t))
         (raw-mute (nth-value 1 (cl-ppcre:scan-to-strings "Mute: ([a-z]+)" raw-output)))
         (raw-volume (nth-value 1 (cl-ppcre:scan-to-strings "Volume: .+/\\s+(\\d+).+/.+/\\s+(\\d+).+/" raw-output)))
         (mutedp (string= (svref raw-mute 0) "yes"))
         (vol%-l (parse-integer (svref raw-volume 0)))
         (vol%-r (parse-integer (svref raw-volume 1))))
    (values mutedp vol%-l vol%-r)))

(defun display-current-volume ()
  "Graphically display the current volume state."
  (multiple-value-bind (mutedp left% right%)
      (current-volume-settings)
    (let ((*record-last-msg-override* t))
      (message "Volume: ~:[~;^1MUTE^n~] [~D%/~D%]" mutedp left% right%))))

(defcommand vol+ (dvol force) ((:number "Delta % (can be negative): ") (:y-or-n "Override volume limits? "))
  "Change the volume by `DV' percent, possibly going over 100% if `FORCE' is T."
  (multiple-value-bind (mutedp left% right%)
      (current-volume-settings)
    (declare (ignore mutedp))
    (let* ((current (max left% right%))
           (target (+ current dvol))
           (final (if force
                      (max 0 target)
                      (clamp target 0 100))))
      (run-shell-command (format nil "pactl set-sink-volume 0 ~D%" final))))
  (display-current-volume))

(defcommand vol-mute () ()
  "Toggle mute of volume."
  (run-shell-command "pactl set-sink-mute 0 toggle" t)
  (display-current-volume))

;;; Within bounds
(define-key stumpwm:*top-map* (stumpwm:kbd "XF86AudioLowerVolume") "vol+ -5 n")
(define-key stumpwm:*top-map* (stumpwm:kbd "XF86AudioRaiseVolume") "vol+ 5 n")
(define-key stumpwm:*top-map* (stumpwm:kbd "C-XF86AudioLowerVolume") "vol+ -1 n")
(define-key stumpwm:*top-map* (stumpwm:kbd "C-XF86AudioRaiseVolume") "vol+ 1 n")
(define-key stumpwm:*top-map* (stumpwm:kbd "S-XF86AudioLowerVolume") "vol+ -25 n")
(define-key stumpwm:*top-map* (stumpwm:kbd "S-XF86AudioRaiseVolume") "vol+ 25 n")

;;; Outside bounds
(define-key stumpwm:*top-map* (stumpwm:kbd "M-XF86AudioLowerVolume") "vol+ -5 y")
(define-key stumpwm:*top-map* (stumpwm:kbd "M-XF86AudioRaiseVolume") "vol+ 5 y")
(define-key stumpwm:*top-map* (stumpwm:kbd "M-C-XF86AudioLowerVolume") "vol+ -1 y")
(define-key stumpwm:*top-map* (stumpwm:kbd "M-C-XF86AudioRaiseVolume") "vol+ 1 y")
(define-key stumpwm:*top-map* (stumpwm:kbd "M-S-XF86AudioLowerVolume") "vol+ -25 y")
(define-key stumpwm:*top-map* (stumpwm:kbd "M-S-XF86AudioRaiseVolume") "vol+ 25 y")

;;; Muting
(define-key stumpwm:*top-map* (stumpwm:kbd "XF86AudioMute") "vol-mute")


;; (ql:quickload :clx-truetype)
;; (xft:cache-fonts)
;; (ql:quickload :ttf-fonts)
;; (load-module "ttf-fonts")
;; (set-font (make-instance 'xft:font
;;                          :family "Hack Nerd Font Mono"
;;                          :subfamily "Regular"
;;                          :size 9))
;; (set-font (make-instance 'xft:font
;;                          :family "DejaVu Sans Mono"
;;                          :subfamily "Book"
;;                          :size 7))

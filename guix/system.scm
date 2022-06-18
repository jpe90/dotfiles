;; This is an operating system configuration generated
;; by the graphical installer.

(use-modules
 (gnu)
 (nongnu packages linux)
 (nongnu system linux-initrd)
 (gnu services ssh))

(use-service-modules
 cups
 desktop
 networking
 ssh
 xorg)

(define %user "solaire")

(operating-system
 (kernel linux)
 (initrd microcode-initrd)
 (firmware (list linux-firmware))
 (locale "en_US.utf8")
 (timezone "America/New_York")
 (keyboard-layout (keyboard-layout "us"))
 (host-name "Artemis")
 (users (cons* (user-account
                (name "solaire")
                (comment "Solaire")
                (group "users")
                (home-directory "/home/solaire")
                (supplementary-groups
                 '("wheel" "netdev" "audio" "video")))
               %base-user-accounts))
 (sudoers-file
     (plain-file "sudoers"
                 (string-append (plain-file-content %sudoers-specification)
                                (format #f "~a ALL = NOPASSWD: ALL~%"
                                        %user))))
  (packages
  (append
   (list
    (specification->package "i3-wm")
    (specification->package "i3status")
    ;; (specification->package "st-eskin")
    ;; (specification->package "dwm-with-windows-key")
    ;; (specification->package "st-guixrus")
    ;; (specification->package "dwm")
    (specification->package "dmenu")
    (specification->package "alacritty")
    ;; (specification->package "st")
    (specification->package "nss-certs")
    (specification->package "vim")
         ;; (specification->package "emacs")
    (specification->package "ntfs-3g")
    (specification->package "git")
    (specification->package "git:send-email")
    (specification->package "rlwrap")
    (specification->package "mosh")
    (specification->package "hare-toolchain")
    )
   %base-packages))
 (services
  (append
   (list 
    ;; (service gnome-desktop-service-type)
    (bluetooth-service #:auto-enable? #t)
    (service openssh-service-type)
    (set-xorg-configuration
     (xorg-configuration
      (keyboard-layout keyboard-layout))))
   %desktop-services))
 (bootloader
  (bootloader-configuration
   (bootloader grub-efi-bootloader)
   (targets (list "/boot/efi"))
   (keyboard-layout keyboard-layout)))
 (file-systems
  (cons* (file-system
          (mount-point "/")
          (device
           (uuid "50ac0b1a-012e-4d5b-9ebb-9f0690023f53"
                 'ext4))
          (type "ext4"))
         (file-system
          (mount-point "/boot/efi")
          (device (uuid "2070-E661" 'fat32))
          (type "vfat"))
         %base-file-systems)))

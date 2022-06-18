;E; This "home-environment" file can be passed to 'guix home reconfigure'
;; to reproduce the content of your profile.  This is "symbolic": it only
;; specifies package names.  To reproduce the exact same profile, you also
;; need to capture the channels being used, as returned by "guix describe".
;; See the "Replicating Guix" section in the manual.

(use-modules
  (gnu home)
  (gnu packages)
  (gnu services)
  (gnu home services)
  (gnu services ssh)
  (guix gexp)
  (gnu home services shells))

(home-environment
  (packages
    (map (compose list specification->package+output)
         '("firefox" "artanis" "nnn" "zathura" "emacs-native-comp" "emacs-guix" "htop"
           "tealdeer" "sbcl" "fzf" "xclip"  "haunt" "tmux" "ripgrep" "emacs-ggtags"
           "global" "bat" "mu" "offlineimap3" "the-silver-searcher" "weechat"
           "mpv" "neovim" "kakoune")))
  (services
    (list (service
            home-bash-service-type
            (home-bash-configuration
              (aliases
                '(("grep" . "grep --color=auto")
                  ("ll" . "ls -l")
                  ("ls" . "ls -p --color=auto")
                  ("uh" . "guix home reconfigure ~/src/guix-config/home-configuration.scm")
                  ("us" . "sudo guix system reconfigure ~/.config/guix/system.scm")
                  ("butthash" . "guix hash -rx .")))
              (bashrc
                (list (local-file
                        "/home/solaire/src/guix-config/.bashrc"
                        "bashrc")
                      (local-file
                        "/home/solaire/src/guix-config/nnn.sh"
                        "nnn")))
              (bash-profile
                (list (local-file
                        "/home/solaire/src/guix-config/.bash_profile"
                        "bash_profile")))))
          (simple-service 'env-vars-service
                          home-environment-variables-service-type
                          `(("EDITOR" . "emacs")
                            ("BAT_THEME" . "GitHub"))))))

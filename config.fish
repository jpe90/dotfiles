#!/usr/bin/env fish
set fish_greeting

#set -gx TERM xterm-256color
set -gx MOZ_ENABLE_WAYLAND 1
set -gx _JAVA_AWT_WM_NONREPARENTING 1
set -gx EDITOR "nvim"
#set -gx JAVA_HOME "/usr/lib/jvm/default"

# guix
set -gx GUIX_PROFILE $HOME/.guix-profile
set -gx GUIX_LOCPATH $HOME/.guix-profile/lib/locale
set -gx SSL_CERT_DIR $HOME/.guix-profile/etc/ssl/certs
set -gx SSL_CERT_FILE $HOME/.guix-profile/etc/ssl/certs/ca-certificates.crt
set -gx GIT_SSL_CAINFO $SSL_CERT_FILE

# need fisher and https://github.com/oh-my-fish/plugin-foreign-env
fenv source $GUIX_PROFILE/etc/profile
fenv source $HOME/.nix-profile/etc/profile.d/hm-session-vars.sh

# aliases
alias em "TERM=xterm-256color emacs -nw -q --load ~/.emacs.d/custom_lisp/quick_init.el"
alias org "em ~/notes/orgmode/todo.org"
alias zth "zathura --fork"
alias vsc "code --enable-features=UseOzonePlatform --ozone-platform=wayland"

# ghcup-env
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
test -f /home/solaire/.ghcup/env ; and set -gx PATH $HOME/.cabal/bin /home/solaire/.ghcup/bin $PATH

# dart
set -gx PATH $HOME/.pub-cache/bin $PATH

# stack
test -f /home/solaire/.stack/config.yaml ; and set -gx PATH $HOME/.local/bin $PATH

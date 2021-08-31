# ghcup-env
set -q GHCUP_INSTALL_BASE_PREFIX[1]; or set GHCUP_INSTALL_BASE_PREFIX $HOME
test -f /home/solaire/.ghcup/env ; and set -gx PATH $HOME/.cabal/bin /home/solaire/.ghcup/bin $PATH
set -gx PATH $HOME/.local/bin $PATH
set -gx _JAVA_AWT_WM_NONREPARENTING 1
set -gx MOZ_ENABLE_WAYLAND 1
set -gx EDITOR /usr/bin/kak
set -gx TERMINAL /usr/bin/alacritty
set -gx IHP_EDITOR code --goto
fish_add_path /opt/cuda/bin

alias rgc="rg --column --line-number --hidden --ignore-case --no-heading --color=always"


#venv

if set -q VIRTUAL_ENV
    echo -n -s (set_color -b blue white) "(" (basename "$VIRTUAL_ENV") ")" (set_color normal) " "
end

alias venv="source venv/bin/activate.fish"


# start X at login
#if status --is-login
#    if test \(-z "$DISPLAY"\) -a \(-z "$SSH_CLIENT"\) -a \(-z "$SSH_TTY"\)
#        exec startx
#    end
#end
# if status is-login
#     if not set -q SSH_TTY
#       if test -z "$DISPLAY" -a "$XDG_VTNR" = 1
#           exec startx -- -keeptty
#       end
#     end
# end

if set -q SSH_TTY
  set -g fish_color_host brred
end

direnv hook fish | source

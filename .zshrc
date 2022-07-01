#!/bin/zsh
export FZF_DEFAULT_OPTS='--height 60% --reverse'
export FZF_COMPLETION_TRIGGER='~~'
  
# export PATH="$HOME/.jenv/bin:$PATH"
# export PATH="$HOME/Development/Datomic/datomic-pro-1.0.6344/bin:$PATH"
# export PATH="$HOME/Development/scripts:$PATH"
# export PATH="/opt/homebrew/bin:$PATH"
# export PATH="`pwd`/flutter/bin:$PATH"
# export ANDROID_SDK_ROOT="/Users/jon/Library/Android/sdk"
# export CHROME_EXECUTABLE="/Applications/Brave Browser.app/Contents/MacOS/Brave Browser"
# export PATH="/Users/jon/Development/Libraries/kitty/kitty.app/Contents/MacOS:$PATH"
export EDITOR=kak
export KAKOUNE_POSIX_SHELL=/bin/dash
eval "$(jenv init -)"

# aliases

# configure history

HISTSIZE=5000
HISTFILE=~/.zsh_history
SAVEHIST=5000
setopt appendhistory
setopt sharehistory

source $(brew --prefix)/share/zsh-autosuggestions/zsh-autosuggestions.zsh
# source $(brew --prefix)/share/zsh-syntax-highlighting/zsh-syntax-highlighting.zsh

zstyle ':completion:*' matcher-list '' 'm:{a-zA-Z}={A-Za-z}' 'r:|=*' 'l:|=* r:|=*'

autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

# autoload -Uz promptinit
# promptinit

n ()
{
    # Block nesting of nnn in subshells
    if [ -n $NNNLVL ] && [ "${NNNLVL:-0}" -ge 1 ]; then
        echo "nnn is already running"
        return
    fi

    # The behaviour is set to cd on quit (nnn checks if NNN_TMPFILE is set)
    # To cd on quit only on ^G, either remove the "export" as in:
    #    NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    #    (or, to a custom path: NNN_TMPFILE=/tmp/.lastd)
    # or, export NNN_TMPFILE after nnn invocation
    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

    nnn -e "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}


# BEGIN_KITTY_SHELL_INTEGRATION
if test -e "$HOME/Development/Libraries/kitty/shell-integration/kitty.zsh"; then source "$HOME/Development/Libraries/kitty/shell-integration/kitty.zsh"; fi
# END_KITTY_SHELL_INTEGRATION

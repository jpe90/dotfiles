# aliases 

alias dtags='dart_ctags -l -o tags'
alias em='sudo emerge --verbose --ask'
alias onedrive='rclone mount onedrive: /home/solaire/mnt/onedrive'
alias emupg='sudo emerge -uDU --keep-going --with-bdeps=y @world'
alias dag='git log --all --graph --decorate'
alias psudo='sudo env "PATH=$PATH"'
alias sld='feh --bg-fill ~/Pictures/wallpapers/landscape/r.jpg ~/Pictures/wallpapers/landscape/l.jpg'

# configure history

HISTSIZE=5000
HISTFILE=~/.zsh_history
SAVEHIST=5000
setopt appendhistory
setopt sharehistory

autoload -U colors && colors

export PATH="$PATH:/home/solaire/Development/GentooAdminScripts"

# TODO: install android SDK
# export PATH="$PATH:/home/solaire/Android/Sdk/platform-tools:/home/solaire/Android/Sdk/tools/bin"

# set FZF to use fd by default to respect gitignore
# TODO: install FZF
# export FZF_DEFAULT_COMMAND='fd --type f'

# TODO: install flutter
# Flutter webdev tool
# export PATH="$PATH":"$HOME/.pub-cache/bin"

# autocomplete
autoload -U compinit
zstyle ':completion:*' menu select
zmodload zsh/complist
compinit
_comp_options+=(globdots)		# Include hidden files.

source ~/.git-prompt.sh

# vi mode
bindkey -v
export KEYTIMEOUT=1

# Use vim keys in tab complete menu:
bindkey -M menuselect 'h' vi-backward-char
bindkey -M menuselect 'k' vi-up-line-or-history
bindkey -M menuselect 'l' vi-forward-char
bindkey -M menuselect 'j' vi-down-line-or-history
bindkey -v '^?' backward-delete-char

# Change cursor shape for different vi modes.
function zle-keymap-select {
  if [[ ${KEYMAP} == vicmd ]] ||
     [[ $1 = 'block' ]]; then
    echo -ne '\e[1 q'
  elif [[ ${KEYMAP} == main ]] ||
       [[ ${KEYMAP} == viins ]] ||
       [[ ${KEYMAP} = '' ]] ||
       [[ $1 = 'beam' ]]; then
    echo -ne '\e[5 q'
  fi
}
zle -N zle-keymap-select
zle-line-init() {
    zle -K viins # initiate `vi insert` as keymap (can be removed if `bindkey -V` has been set elsewhere)
    echo -ne "\e[5 q"
}
zle -N zle-line-init
echo -ne '\e[5 q' # Use beam shape cursor on startup.
preexec() { echo -ne '\e[5 q' ;} # Use beam shape cursor for each new prompt.

# prompt theme system

autoload -Uz promptinit
promptinit
setopt PROMPT_SUBST
GIT_PS1_SHOWDIRTYSTATE="y"
GIT_PS1_SHOWCOLORHINTS="y"
GIT_PS1_SHOWUPSTREAM="verbose git"
GITPST_DESCRIBE_STYLE="branch"
precmd () { __git_ps1 "%F{cyan}%n%f" " %~ $ " " %s" }

# Load zsh-syntax-highlighting; should be last.
 source /usr/share/zsh/site-functions/zsh-syntax-highlighting.zsh 2>/dev/null
 




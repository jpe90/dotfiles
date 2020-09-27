# aliases 

alias dtags='dart_ctags -l -o tags'
alias em='sudo emerge --quiet --ask'
alias onedrive='rclone mount onedrive: /home/solaire/mnt/onedrive'

autoload -U colors && colors

# android SDK

export PATH="$PATH:/home/solaire/Android/Sdk/platform-tools:/home/solaire/Android/Sdk/tools/bin"

# set FZF to use fd by default to respect gitignore
# TODO: install FZF
# export FZF_DEFAULT_COMMAND='fd --type f'

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
#prompt gentoo
setopt PROMPT_SUBST
GIT_PS1_SHOWDIRTYSTATE="y"
precmd () { __git_ps1 "%n" ":%~$ " "|%s" }
PS1='[%n@%m %c$(__git_ps1 " (%s)")]\$ '

# Load zsh-syntax-highlighting; should be last.
 source /usr/share/zsh/site-functions/zsh-syntax-highlighting.zsh 2>/dev/null
 


# TODO: install flutter
# Flutter webdev tool
export PATH="$PATH":"$HOME/.pub-cache/bin"


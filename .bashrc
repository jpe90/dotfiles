#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

alias ls='ls --color=auto'
alias examod="exa -lar -s mod"
alias dotfiles="cd ~/development/dotfiles/"
alias blog="cd ~/development/web/blog/"
alias csmo="cd ~/development/c/cosmo2/"
alias vps="ssh"
alias opennewterm="st >/dev/null 2>&1 & disown"
alias ec="emacsclient -nw"

PS1='[\u@\h \W]\$ '
export EDITOR=emacsclient -nw
export BROWSER=firefox
export PATH="$HOME/.local/bin:$PATH"
export PATH="$HOME/development/shell:$PATH"
export PATH="$HOME/.cargo/bin:$PATH"
export _JAVA_AWT_WM_NONREPARENTING=1

n ()
{
    # Block nesting of nnn in subshells
    if [[ "${NNNLVL:-0}" -ge 1 ]]; then
        echo "nnn is already running"
        return
    fi

    # The behaviour is set to cd on quit (nnn checks if NNN_TMPFILE is set)
    # If NNN_TMPFILE is set to a custom path, it must be exported for nnn to
    # see. To cd on quit only on ^G, remove the "export" and make sure not to
    # use a custom path, i.e. set NNN_TMPFILE *exactly* as follows:
    #     NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"
    export NNN_TMPFILE="${XDG_CONFIG_HOME:-$HOME/.config}/nnn/.lastd"

    # Unmask ^Q (, ^V etc.) (if required, see `stty -a`) to Quit nnn
    # stty start undef
    # stty stop undef
    # stty lwrap undef
    # stty lnext undef

    # The backslash allows one to alias n to nnn if desired without making an
    # infinitely recursive alias
    \nnn "$@"

    if [ -f "$NNN_TMPFILE" ]; then
            . "$NNN_TMPFILE"
            rm -f "$NNN_TMPFILE" > /dev/null
    fi
}


[ -f ~/.fzf.bash ] && source ~/.fzf.bash

source "%val{config}/plugins/plug.kak/rc/plug.kak"
plug "andreyorst/plug.kak" noload

plug "andreyorst/fzf.kak" config %{
	map global normal <c-p> ': fzf-mode<ret>'
} defer "fzf-grep" %{
    set-option global fzf_grep_command 'rg'
}

plug "andreyorst/smarttab.kak" defer smarttab %{
    set-option global softtabstop 4
} config %{
    set-option global expandtab
    hook global WinSetOption filetype=(dart|haskell|python|markdown) softtabstop 2
}

set-option global indentwidth 4
set-option global tabstop 4

hook global WinSetOption filetype=haskell %{
  set-option window formatcmd 'fourmolu -i'
  set-option window indentwidth 2
  set-option window tabstop 2
}

hook global WinSetOption filetype=dart %{
  set-option window indentwidth 2
  set-option window tabstop 2
}

hook global WinSetOption filetype=python %{
  set-option window indentwidth 2
  set-option window tabstop 2
}
hook global WinSetOption filetype=markdown %{
  set-option window indentwidth 2
  set-option window tabstop 2
}

eval %sh{kak-lsp --kakoune -s $kak_session}  # Not needed if you load it with plug.kak.

lsp-enable
hook global WinCreate .* %{addhl number_lines -relative}
# map global user l %{: enter-user-mode lsp<ret>} -docstring "LSP mode"
map global normal '#' ': comment-line<ret>'                   -docstring 'comment-line'
map global normal '=' ': format<ret>'                         -docstring 'format'

map global normal <c-l> ': enter-user-mode lsp<ret>' -docstring "LSP mode"

add-highlighter global/ number-lines -relative
# colorscheme plain
colorscheme tomorrow-night
# colorscheme gruvbox
# colorscheme plainplus


# face global PrimaryCursor grey,white

map global insert <tab> '<a-;><gt>'
map global insert <s-tab> '<a-;><lt>'

hook global RawKey <mouse:press:middle:.*> %{ exec !xclip<space>-o<ret> }

plug "lePerdu/kakboard" %{
        hook global WinCreate .* %{ kakboard-enable }
}

hook global InsertCompletionShow .* %{
    try %{
        # this command temporarily removes cursors preceded by whitespace;
        # if there are no cursors left, it raises an error, does not
        # continue to execute the mapping commands, and the error is eaten
        # by the `try` command so no warning appears.
        execute-keys -draft 'h<a-K>\h<ret>'
        map window insert <tab> <c-n>
        map window insert <s-tab> <c-p>
        hook -once -always window InsertCompletionHide .* %{
            unmap window insert <tab> <c-n>
            unmap window insert <s-tab> <c-p>
        }
    }
}

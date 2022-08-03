-- load standard vis module, providing parts of the Lua API
require('vis')
require('plugins/vis-commentary')
require('mail_detect')
require('themes/min')
plugin_vis_open = require('plugins/vis-fzf-open')

vis.events.subscribe(vis.events.WIN_OPEN, function(win)
    -- Your per window configuration options e.g.
    vis:command('set autoindent on')
    vis:command('set colorcolumn 80')
    vis:command('set cursorline')
    vis:command('set show-spaces on')
    vis:command('set show-tabs')
    -- vis:command('set expandtab on')
    vis:command('set number')
    vis:command('set relativenumbers')
    vis:command('set show-spaces off')
    vis:command('set show-tabs off')
    vis:command('set tabwidth 4')
    vis:command('set theme min')
    -- vis:command('set theme dark-16')
    -- vis:command('set theme acme')
    -- vis:command('set ignorecase on')
    vis:command('set shell "/bin/dash"')
    if win.syntax == 'hare' then
        vis:command('set expandtab off')
    end
    if win.syntax == 'markdown' then
        vis:command('set colorcolumn 0')
    end
end)

-- Arguments passed to fzf (default: "")


-- Path to the fzf executable (default: "fzf")
plugin_vis_open.fzf_path = (
    "FZF_DEFAULT_COMMAND='ag --hidden --ignore .git -g \"\"' fzf"
    )

fzf_file_args = string.gsub([[
    --bind=$my_fzf_key_bindings \
    --color info:108,prompt:109,spinner:108,pointer:168,marker:168 \
    --delimiter / --nth -1 \
    --height=70% \
    --inline-info \
    --no-mouse \
    --preview-window=right:70%:+{2}-/2 \
    --preview="(
        bat --style=changes,grid,numbers --color=always {} ||
        cat {}
    ) 2> /dev/null | head -1000"
]], '%$([%w_]+)', {
    my_fzf_key_bindings=table.concat({
        "alt-j:preview-down",
        "alt-k:preview-up",
        "ctrl-f:preview-page-down",
        "ctrl-b:preview-page-up",
        "?:toggle-preview",
        "alt-w:toggle-preview-wrap",
        "ctrl-z:clear-screen"
    }, ",")
})

vis:command_register('sw', function(argv)
    vis:command('set show-spaces on')
    vis:command('set show-tabs on')
end, "Turn whitespace on")

-- Arguments passed to fzf (default: "")
plugin_vis_open.fzf_args = fzf_file_args


-- Mapping configuration example
vis.events.subscribe(vis.events.INIT, function()
vis:command('map! normal <C-p> :fzf<Enter>')
end)

vis:map(vis.modes.VISUAL, " y", '"+y')
vis:map(vis.modes.NORMAL, " p", '"+p')

vis.events.subscribe(vis.events.FILE_SAVE_PRE, function()
	trim_whitespace()
end)

-- Delete trailing whitespace
function trim_whitespace()
	local name = vis.win.file.name
	if name and (name:match("%.patch$") or name:match("%.eml$")) then
		return
	end

	local vp = vis.win.viewport

	local sel = vis.win.selection
	local s_line = sel.line
	local s_col = sel.col

	local lines = vis.win.file.lines
	for i=1, #lines do
		local new_line = lines[i]:gsub("^(.-)%s*$", "%1")
		if lines[i] ~= new_line then
			lines[i] = new_line
		end
	end

	sel:to(s_line, s_col)
	vis.win.viewport = vp
end


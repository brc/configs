" Disable GetYamlIndent() function from salt-vim plugin
" (it messes up my pretty Jinja comments, yo!)
setlocal indentexpr=

" Find git repo root dir.
" Using the systemlist() builtin will split the output lines into an array,
" which prunes the newline/null character from the end of the string for us.
" TODO: check for error
let s:gitroot = systemlist('git rev-parse --show-toplevel')[0]

" set makeprg to tox
" TODO: check that tox.ini exists first
let &l:makeprg = 'tox -c ' . s:gitroot . '/test/tox.ini'

" tpope's Dispatch plugin provides 'Make' command
map <buffer> <F6> :Make<CR>

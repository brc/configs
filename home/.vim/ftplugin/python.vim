"setlocal omnifunc=pythoncomplete#Complete
let python_space_error_highlight=1

" Find git repo root dir.
"
" Using the systemlist() builtin will split the output lines into an array,
" which prunes the newline/null character from the end of the string for us.
" TODO: check for error
let s:gitroot = systemlist('git rev-parse --show-toplevel')[0]

" set makeprg to tox
" TODO: check that tox.ini exists first
execute 'setlocal makeprg=tox\ -c\ ' . s:gitroot . '/tox.ini'

" tpope's Dispatch plugin provides 'Make' command
map <buffer> <F6> :Make<CR>

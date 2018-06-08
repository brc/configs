"setlocal omnifunc=pythoncomplete#Complete
let python_space_error_highlight = 1

" Find git repo root dir.
"
" Using the systemlist() builtin will split the output lines into an array,
" which prunes the newline/null character from the end of the string for us.
let s:gitroot = systemlist('git rev-parse --show-toplevel')[0]

" set makeprg to tox
if s:gitroot !~ '^fatal:'
    if filereadable(s:gitroot . '/tox.ini')
        execute 'setlocal makeprg=tox\ -c\ ' . s:gitroot . '/tox.ini'
        " tpope's Dispatch plugin provides 'Make' command
        nnoremap <buffer> <F10> :Make<CR>
    endif
endif

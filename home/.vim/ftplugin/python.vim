"setlocal omnifunc=pythoncomplete#Complete
let python_space_error_highlight = 1

" Find git repo root dir.
"
" Using the systemlist() builtin will split the output lines into an array,
" which prunes the newline/null character from the end of the string for us.
let b:gitroot = systemlist('git rev-parse --show-toplevel')[0]
if b:gitroot =~ '^fatal:'
    unlet b:gitroot
endif

compiler pylint

" set makeprg to tox
"if exists("b:gitroot")
"    if filereadable(b:gitroot . '/tox.ini')
"        "execute 'setlocal makeprg=tox\ -c\ ' . b:gitroot . '/tox.ini'
"        " tpope's Dispatch plugin provides 'Make' command
"        nnoremap <buffer> <F10> :Make<CR>
"    endif
"endif

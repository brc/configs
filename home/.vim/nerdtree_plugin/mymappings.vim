" make spacebar do same thing as 'o' key (g:NERDTreeMapActivateNode)
" XXX following command doesn't work from this file
" nnoremap <buffer> <silent> <Space> :call nerdtree#invokeKeyMap('o')<CR>

" Ripped these function contents out of nerdtree.vim --
" couldn't figure out how to reuse the existing ones (vim script newb)
"
" make 'l' key open directories (but not toggle them)
"call NERDTreeAddKeyMap({
"        \ 'key': 'l',
"        \ 'scope': 'DirNode',
"        \ 'callback': 'NERDTreeOpenDir',
"        \ 'quickhelpText': 'open dir' })
"
"function! NERDTreeOpenDir(node)
"    call a:node.open({'reuse': 1})
"    call nerdtree#renderView()
"endfunction

" make 'h' key close directories (but not toggle them)
"call NERDTreeAddKeyMap({
"        \ 'key': 'h',
"        \ 'scope': 'DirNode',
"        \ 'callback': 'NERDTreeCloseDir',
"        \ 'quickhelpText': 'close dir' })
"
"function! NERDTreeCloseDir(node)
"    call a:node.close()
"    call nerdtree#renderView()
"endfunction

call NERDTreeAddKeyMap({
        \ 'key': 'yy',
        \ 'callback': 'NERDTreeYankCurrentNode',
        \ 'quickhelpText': 'put full path of current node into the default register' })

function! NERDTreeYankCurrentNode()
    let n = g:NERDTreeFileNode.GetSelected()
    if n != {}
        "call setreg('"', (fnamemodify(n.path.str(), ':.')))
        call setreg('"', n.path.str())
    endif
endfunction

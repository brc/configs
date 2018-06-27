setlocal encoding=utf-8
setlocal textwidth=0
setlocal wrap
setlocal linebreak
setlocal matchpairs+=<:>

nnoremap <buffer> k gk
nnoremap <buffer> j gj
nnoremap <buffer> <Up> gk
nnoremap <buffer> <Down> gj
nnoremap <buffer> 0 g0
nnoremap <buffer> ^ g^
nnoremap <buffer> $ g$
inoremap <buffer> <Up> <C-O>gk
inoremap <buffer> <Down> <C-O>gj
vnoremap <buffer> k gk
vnoremap <buffer> j gj
vnoremap <buffer> <Up> gk
vnoremap <buffer> <Down> gj
vnoremap <buffer> 0 g0
vnoremap <buffer> ^ g^
vnoremap <buffer> $ g$

imap <buffer> <LocalLeader>h1 ==  ==<ESC>bhha
imap <buffer> <LocalLeader>h2 ===  ===<ESC>bhha
imap <buffer> <LocalLeader>h3 ====  ====<ESC>bhha
imap <buffer> <LocalLeader>h4 =====  =====<ESC>bhha

imap <buffer> <LocalLeader>br <br>

imap <buffer> <LocalLeader>tt <tt>
" imap <buffer> .tt </tt>

map <buffer> <LocalLeader>tt i<tt><ESC>
" map <buffer> .tt i</tt><ESC>

" imap <buffer> ..tt <tt></tt><ESC>2ba

imap <buffer> <LocalLeader>l <ESC>A

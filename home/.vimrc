execute pathogen#infect()
runtime macros/matchit.vim

set autoindent
set backspace=indent,eol,start
set backupcopy=auto
set colorcolumn=80
set expandtab
set fileformat=unix
set grepprg=grep\ -rsiI\ --exclude-dir=.git
set history=15
set hlsearch
set ignorecase
set listchars+=precedes:<,extends:>
set modeline
"set mouse=a
set nowrap
set ruler
set scrolloff=5
set shiftround
set shiftwidth=4
set showmatch
set showmode
set sidescroll=10
set sidescrolloff=20
set smartcase
set smarttab
set softtabstop=4
set tabstop=8
"set textwidth=0
set undolevels=75
set visualbell
set wildmenu
set wildmode=list,longest,full
set writebackup

set cpoptions-=J  " sentences do NOT have to end with two spaces
set cpoptions-=u  " vim undo, not vi-compatible
set cpoptions-=l  " backslash in a range /[] is magical (not literal)
set cpoptions-=y  " yank command cannot be redone with '.'

" Tell vim to remember certain things when we exit
"  '10  :  marks will be remembered for up to 10 previously edited files
"  "100 :  will save up to 100 lines for each register
"  :50  :  up to 20 lines of command-line history will be remembered
"  %    :  saves and restores the buffer list
"  n... :  where to save the viminfo files
set viminfo='10,\"100,:50,%,h,n~/.viminfo,f

" Global vars
"let g:leave_my_textwidth_alone = 1 
let maplocalleader = ","

"set guioptions-=m
"set guioptions-=M
"set guioptions-=T
"set guioptions-=a
"set guioptions+=c
"set guifont=vt100:h10

" Emacs-style keybindings for command line
"
:cnoremap <C-A>		<Home>
:cnoremap <C-E>		<End>
:cnoremap <C-B>		<Left>
:cnoremap <C-F>		<Right>
:cnoremap <C-D>		<Del>
:cnoremap <C-N>		<Down>
:cnoremap <C-P>		<Up>
:cnoremap <Esc><C-B>	<S-Left>
:cnoremap <Esc><C-F>	<S-Right>
:cnoremap <C-U>         <C-E><C-U>

filetype plugin indent on
syntax on

" Maybe configure this so it knows xrandr is using the VGA output?
"
"if exists("$SSH_CLIENT")
"    color evening
"else
    color ir_black
    "color delek
    hi Search  term=underline cterm=underline,bold ctermfg=yellow ctermbg=magenta
    hi WarningMsg term=standout ctermfg=yellow ctermbg=blue
"endif

"hi Comment      term=none       ctermfg=cyan       cterm=bold
"hi Constant     term=underline  ctermfg=magenta    cterm=none
"hi Identifier   term=underline  ctermfg=green      cterm=none
"hi Statement    term=bold       ctermfg=yellow     cterm=bold
"hi PreProc      term=underline  ctermfg=red   cterm=bold
"hi Type         term=underline  ctermfg=yellow     cterm=bold
"hi Special      term=bold       ctermfg=red        cterm=bold
"hi Nontext      term=bold       ctermfg=blue       cterm=bold
"hi Normal       ctermfg=darkgreen
"hi Search        term=underline cterm=underline ctermfg=magenta ctermbg=none
hi ColorColumn ctermbg=blue ctermfg=yellow

if &diff
    "colorscheme some_other_scheme
    highlight DiffAdd term=reverse cterm=bold ctermbg=green ctermfg=black
    highlight DiffChange term=reverse cterm=none ctermbg=cyan ctermfg=black
    highlight DiffText term=reverse cterm=none ctermbg=blue ctermfg=white
    highlight DiffDelete term=reverse cterm=none ctermbg=black ctermfg=red
endif

"
" restore cursor position when opening file
"
function! ResCur()
  if line("'\"") <= line("$")
    normal! g`"
    return 1
  endif
endfunction

augroup resCur
  autocmd!
  autocmd BufWinEnter * call ResCur()
augroup END

"
" Set `paste' option for *THIS* single insert only
" (unimpairedPaste from Tim Pope)
"
function! Setup_paste() abort
  let s:paste = &paste
  set paste
endfunction

nnoremap <silent> yp :call <SID>Setup_paste()<CR>a
nnoremap <silent> yP :call <SID>Setup_paste()<CR>i
nnoremap <silent> yo :call <SID>Setup_paste()<CR>o
nnoremap <silent> yO :call <SID>Setup_paste()<CR>O
nnoremap <silent> yA :call <SID>Setup_paste()<CR>A
nnoremap <silent> yI :call <SID>Setup_paste()<CR>I
nnoremap <silent> ygi :call <SID>Setup_paste()<CR>gi
nnoremap <silent> ygI :call <SID>Setup_paste()<CR>gI

augroup unimpaired_paste
  autocmd!
  autocmd InsertLeave *
        \ if exists('s:paste') |
        \ let &paste = s:paste |
        \ unlet s:paste |
        \ endif
augroup END

" Automatically show global/local quickfix windows
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

" force the quickfix window to the BOTTOM
" (to fix things when tagbar is active)
autocmd FileType qf wincmd J

""" syntax hilighting
map <F1> :syntax on<CR>
map <S-F1> :syntax off<CR>

""" paste/nopaste
map <F2> :set paste<CR>
imap <F2> :call Setup_paste()<CR>
map <S-F2> :set nopaste<CR>

""" edit alternate file
map <F3> :e #<CR>

""" toggle tagbar
nmap <F4> :TagbarToggle<CR>

""" filetype detection
map <F6> :filetype detect<CR>

""" options for writing/quiting files
map <F8> :wq<CR>
map <S-F8> :wqa<CR>

map <F9> :q<CR>
map <S-F9> :qa<CR>

map <F10> :q!
map <S-F10> :qa!

map <F11> :nohl<CR>
map <S-F11> :set hls<CR>

map <F12> :w<CR>
map <S-F12> :wa<CR>

map <S-F3> <F12><F3>

""" invoke ispell
"map <A-F12> :w<CR>:!ispell -x %<CR><CR>:e<CR>


""" pan left/right
map l z10l
map h z10h

""" toggle folds
nnoremap <silent> <Space> @=(foldlevel('.')?'za':"\<Space>")<CR>

""" vimgrep
nnoremap gr :vimgrep //j **<Left><Left><Left><Left><Left>

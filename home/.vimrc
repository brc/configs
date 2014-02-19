" ###########################################################################
" ### LOAD PLUGINS
" ###########################################################################
execute pathogen#infect()
runtime macros/matchit.vim

" ###########################################################################
" ### SET OPTIONS
" ###########################################################################
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

"set guioptions-=m
"set guioptions-=M
"set guioptions-=T
"set guioptions-=a
"set guioptions+=c
"set guifont=vt100:h10

"" Tell vim to remember certain things when we exit
""   '10         marks will be remembered for up to 10 previously edited files
""   "100        will save up to 100 lines for each register
""   :50         up to 50 lines of command-line history will be remembered
""   %           saves and restores buffer list
""   h	        disable 'hlsearch' highlighting when starting
""   f1          store global marks (A-Z and 0-9)
""   n           name used for the viminfo file (must be the last option)
set viminfo='10,\"100,:50,%,h,f1,n~/.viminfo

filetype plugin indent on
syntax on

let maplocalleader = ","


" ###########################################################################
" ### FUNCTIONS / AUTOCOMMANDS
" ###########################################################################
"
" Restore cursor position when opening file
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


" unimpairedPaste from Tim Pope
" (set `paste' for current insert only)
"
function! Setup_paste() abort
  let s:paste = &paste
  set paste
endfunction

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


" ###########################################################################
" ### SYNTAX HIGHLIGHTING
" ###########################################################################
"
"hi Comment      term=none       ctermfg=cyan       cterm=bold
"hi Constant     term=underline  ctermfg=magenta    cterm=none
"hi Identifier   term=underline  ctermfg=green      cterm=none
"hi Statement    term=bold       ctermfg=yellow     cterm=bold
"hi PreProc      term=underline  ctermfg=red   cterm=bold
"hi Type         term=underline  ctermfg=yellow     cterm=bold
"hi Special      term=bold       ctermfg=red        cterm=bold
"hi Nontext      term=bold       ctermfg=blue       cterm=bold
"hi Normal       ctermfg=darkgreen
"hi Search       term=underline cterm=underline ctermfg=magenta ctermbg=none
color ir_black

" TODO: configure this so it checks xrandr for VGA output
"color delek

" Custom highlights
"""""""""""""""""""
hi Search       cterm=underline,bold ctermfg=yellow ctermbg=magenta
hi WarningMsg   ctermfg=yellow ctermbg=blue
hi ColorColumn  ctermbg=blue ctermfg=yellow

" Vimdiff mode
""""""""""""""
if &diff
    "colorscheme <some_scheme> works here
    highlight DiffAdd term=reverse cterm=bold ctermbg=green ctermfg=black
    highlight DiffChange term=reverse cterm=none ctermbg=cyan ctermfg=black
    highlight DiffText term=reverse cterm=none ctermbg=blue ctermfg=white
    highlight DiffDelete term=reverse cterm=none ctermbg=black ctermfg=red
endif


" ###########################################################################
" ### UTF-8
" ###########################################################################
if has("multi_byte")
  set encoding=utf-8
  setglobal fileencoding=utf-8
  "setglobal bomb
  set fileencodings=ucs-bom,utf-8,latin1
endif


" ###########################################################################
" ###########################################################################
" #########  KEYBOARD MAPPING  #############################################
" ###########################################################################
" ###########################################################################
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Fix internal keycodes when running under TERM=screen in tmux
" (sequences wildcarded to make modifiers work as well (<S-F1>, <C-F1>, etc))
"
"       Run `set -g xterm-keys on' in tmux to make this work.
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
if &term =~ '^screen' && exists('$TMUX')
    execute "set <xUp>=\e[1;*A"
    execute "set <xDown>=\e[1;*B"
    execute "set <xRight>=\e[1;*C"
    execute "set <xLeft>=\e[1;*D"
    execute "set <xHome>=\e[1;*H"
    execute "set <xEnd>=\e[1;*F"
    execute "set <Insert>=\e[2;*~"
    execute "set <Delete>=\e[3;*~"
    execute "set <PageUp>=\e[5;*~"
    execute "set <PageDown>=\e[6;*~"
    execute "set <xF1>=\e[1;*P"
    execute "set <xF2>=\e[1;*Q"
    execute "set <xF3>=\e[1;*R"
    execute "set <xF4>=\e[1;*S"
    execute "set <F5>=\e[15;*~"
    execute "set <F6>=\e[17;*~"
    execute "set <F7>=\e[18;*~"
    execute "set <F8>=\e[19;*~"
    execute "set <F9>=\e[20;*~"
    execute "set <F10>=\e[21;*~"
    execute "set <F11>=\e[23;*~"
    execute "set <F12>=\e[24;*~"
endif

" Emacs-style keybindings for command line
""""""""""""""""""""""""""""""""""""""""""
cnoremap <C-A>		<Home>
cnoremap <C-E>		<End>
cnoremap <C-B>		<Left>
cnoremap <C-F>		<Right>
cnoremap <C-D>		<Del>
cnoremap <C-N>		<Down>
cnoremap <C-P>		<Up>
cnoremap <Esc><C-B>	<S-Left>
cnoremap <Esc><C-F>	<S-Right>
cnoremap <C-U>         <C-E><C-U>

" unimpairedPaste
"""""""""""""""""
nnoremap <silent> yp :call <SID>Setup_paste()<CR>a
nnoremap <silent> yP :call <SID>Setup_paste()<CR>i
nnoremap <silent> yo :call <SID>Setup_paste()<CR>o
nnoremap <silent> yO :call <SID>Setup_paste()<CR>O
nnoremap <silent> yA :call <SID>Setup_paste()<CR>A
nnoremap <silent> yI :call <SID>Setup_paste()<CR>I
nnoremap <silent> ygi :call <SID>Setup_paste()<CR>gi
nnoremap <silent> ygI :call <SID>Setup_paste()<CR>gI

""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" NB: Some commands below use raw control characters (^O, ^W, etc)
"""     (ie., don't copy/paste them with a mouse)
""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
"
" NERDTree window
"""""""""""""""""
"toggle
map <F1> :NERDTreeToggle<CR>
imap <F1> :NERDTreeToggle<CR>
"focus (far left)
map <S-F1> 99h
imap <S-F1> 99h

" QuickFix window
"""""""""""""""""
"toggle
map <F2> :call ToggleQuickfixList()<CR>
imap <F2> :call ToggleQuickfixList()<CR>
"focus (very bottom)
map <S-F2> 99j
imap <S-F2> 99j

" MBE window
""""""""""""
"toggle
map <F3> :MBEToggle<CR>
imap <F3> :MBEToggle<CR>
"focus (very top)
map <S-F3> 99k
imap <S-F3> 99k

" Tagbar window
"""""""""""""""
"toggle
map <F4> :TagbarToggle<CR>
imap <F4> :TagbarToggle<CR>
"focus (far right)
map <S-F4> 99l
imap <S-F4> 99l

" Previous window
"""""""""""""""""
map <S-F5> 
imap <S-F5> 

" Paste
"""""""
"toggle
map <F5> :set paste<CR>
map <S-F5> :set nopaste<CR>
"one-time paste for insert-mode
imap <F5> :call Setup_paste()<CR>

"""
"map <F6>

" Writing/quiting files
"""""""""""""""""""""""
map <F8> :wq<CR>
map <S-F8> :wqa<CR>

map <F9> :q<CR>
map <S-F9> :qa<CR>

map <F10> :q!
map <S-F10> :qa!

map <F12> :w<CR>
map <S-F12> :wa<CR>

" Search highlighting
"""""""""""""""""""""
map <F11> :nohl<CR>
imap <F11> :nohl<CR>
map <S-F11> :set hls<CR>
imap <S-F11> :set hls<CR>

" Pan left/right
""""""""""""""""
map l z10l
map h z10h

" Toggle folds
""""""""""""""
nnoremap <silent> <Space> @=(foldlevel('.')?'za':"\<Space>")<CR>

" Vimgrep
"""""""""
nnoremap gr :vimgrep //j **<Left><Left><Left><Left><Left>

" Prevent window death
""""""""""""""""""""""
nnoremap <C-w>o :echoerr "Go fuck yourself :-)"<CR>
nnoremap <C-w><C-o> :echoerr "Go fuck yourself :-)"<CR>


" ###########################################################################
" ###########################################################################
" #########  PLUGIN CONFIGURATION  ##########################################
" ###########################################################################
" ###########################################################################
"
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" NERDTree
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" don't use unicode arrow glyphs
let NERDTreeDirArrows = 0
let NERDTreeHijackNetrw = 1
let NERDTreeShowBookmarks = 1
let NERDTreeShowHidden = 1
let NERDTreeMinimalUI = 1
" swap highlighting of directories and symlinks
hi link NERDTreeDir Macro
hi link NERDTreeLink Directory

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" Tagbar
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" don't use unicode arrow glyphs
let g:tagbar_iconchars = ['+', '-'] 

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" togglelist.vim
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" disable default plugin bindings
let g:toggle_list_no_mappings = 1


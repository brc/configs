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
set diffopt=filler,vertical
set expandtab
set fileformat=unix
set grepprg=grep\ -sIRin\ --exclude-dir=.git\ --exclude-dir=.tox
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
set showcmd
set showmatch
set showmode
set sidescroll=10
set sidescrolloff=20
set smartcase
set smarttab
set softtabstop=4
set tabstop=8
set ttyfast
"set textwidth=0
set undofile
set updatetime=1000
set visualbell
set wildmenu
set wildmode=list,longest,full
set writebackup

"set guioptions-=m
"set guioptions-=M
"set guioptions-=T
"set guioptions-=a
"set guioptions+=c
"set guifont=vt100:h10

"" Tell vim to remember certain things when we exit
""   '100        marks will be remembered for up to 100 previously edited files
""   :500        up to 500 lines of command-line history will be remembered
""   %           saves and restores buffer list
""   h           disable 'hlsearch' highlighting when starting
""   f1          store global marks (A-Z and 0-9)
""   n           name used for the viminfo file (must be the last option)
set viminfo='100,:500,%,h,f1,n~/.viminfo

filetype plugin indent on

" Override whatever 'fo' string filetype plugins set
" (see https://groups.google.com/forum/#!topic/vim_dev/EKDS1PP4rPo)
autocmd FileType * setlocal formatoptions+=qroj

let maplocalleader = ","
syntax on


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

" Automatically show global/local quickfix windows
autocmd QuickFixCmdPost [^l]* nested cwindow
autocmd QuickFixCmdPost    l* nested lwindow

" force the quickfix window to the BOTTOM
" (to fix things when tagbar is active)
autocmd FileType qf wincmd J

" highlight current line in NERDTree
autocmd FileType nerdtree :setl cursorline


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

"color ir_black
"color vividchalk
"color jellybeans
"color inkpot

" Molokai: bring the 256 color version as close as possible to the the default
" (dark) GUI version
"let g:rehash256 = 1

color molokai

" TODO: configure this so it checks xrandr for VGA output
"color delek

" Custom highlights
"""""""""""""""""""
hi Search       cterm=underline,bold ctermfg=yellow ctermbg=magenta
hi WarningMsg   ctermfg=yellow ctermbg=red
"hi ColorColumn  ctermbg=blue ctermfg=yellow

" Vimdiff mode
""""""""""""""
"if &diff
    "colorscheme <some_scheme> works here
    highlight DiffAdd term=reverse cterm=bold ctermbg=green ctermfg=black
    highlight DiffChange term=reverse cterm=none ctermbg=cyan ctermfg=black
    highlight DiffText term=reverse cterm=none ctermbg=blue ctermfg=white
    highlight DiffDelete term=reverse cterm=none ctermbg=black ctermfg=red
"endif


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

" timeout on keymappings: true
" wait half-second for mappings and tenth-second for keycodes
set timeout timeoutlen=500 ttimeoutlen=100

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

" Emacs-style keybindings (for Insert and Command-line modes)
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
noremap! <C-a>      <Home>
noremap! <C-e>      <End>
noremap! <C-b>      <Left>
noremap! <C-f>      <Right>
noremap! <C-u>      <C-e><C-u>
noremap! <C-d>      <Del>
"can't do these on command-line; Insert only (sends Escape):
inoremap <C-k>      <ESC>lC
inoremap <C-t>      <ESC>xhpa
"timeoutlen and ttimeoutlen don't seem to do what i want;
"don't allow these in Insert mode:
cnoremap <ESC>b     <S-Left>
cnoremap <ESC>f     <S-Right>
"don't let these screw up the command-line if pressed on accident:
cmap     <ESC>d     <Nop>
cmap     <C-k>      <Nop>

" Search and replace
""""""""""""""""""""
nnoremap <LocalLeader>ra :%s/<C-r><C-w>//gc<Left><Left><Left>

" Netrw browser
"""""""""""""""
" (like vim-vinegar)
nnoremap - :silent edit <C-R>=empty(expand('%')) ? '.' : expand('%:p:h')<CR><CR>

" QuickFix window
"""""""""""""""""
"toggle/focus (very bottom)
nmap <LocalLeader>ff :call ToggleQuickfixList()<CR>
nmap <LocalLeader>gf 99<C-w>j

"dynamic height
au FileType qf call AdjustWindowHeight(3, 10)
function! AdjustWindowHeight(minheight, maxheight)
  exe max([min([line("$"), a:maxheight]), a:minheight]) . "wincmd _"
endfunction

" MBE window
""""""""""""
"toggle/focus (very top)
nmap <LocalLeader>mm :MBEToggle<CR>
nmap <LocalLeader>gm 99<C-w>k

" Tagbar window
"""""""""""""""
"toggle/focus (far right)
nmap <LocalLeader>tt :TagbarToggle<CR>
nmap <LocalLeader>gt 99<C-w>l

" Window management
"""""""""""""""""""
nmap <LocalLeader>w <c-w>
nmap <LocalLeader>we <c-w>=
nnoremap <LocalLeader>ww <c-w>p

" Paste
"""""""
"toggle
nmap <F1> :set paste<CR>
nmap <S-F1> :set nopaste<CR>
"one-time paste for insert-mode
"inoremap <F1> <C-o>:call Setup_paste()<CR>

" yank
""""""
nmap Y y$

" Writing/quiting files
"""""""""""""""""""""""
map <F4> :w<CR>
imap <F4> <ESC>:w<CR>

map  <F7> :wq<CR>
imap <F7> <ESC>:wq<CR>

map <F5> :q<CR>
map <S-F5> :qa<CR>

map <F8> :q!
map <S-F8> :qa!

" Search highlighting
"""""""""""""""""""""
map <F9> :nohl<CR>
imap <F9> <C-o>:nohl<CR>

map <S-F9> :set hls<CR>
imap <S-F9> <C-o>:set hls<CR>

" Pan left/right
""""""""""""""""
map <ESC>l z10l
map <ESC>h z10h

" Toggle folds
""""""""""""""
nnoremap <silent> <Space> @=(foldlevel('.')?'za':"\<Space>")<CR>
map <LocalLeader>f zi

" Refresh diff highlighting
"""""""""""""""""""""""""""
map <LocalLeader>du :diffupdate<CR>

" Vimgrep
"""""""""
nnoremap gr :vimgrep //j **<Left><Left><Left><Left><Left>
"nnoremap gr :silent grep!  \| redraw!<S-Left><S-Left><S-Left>

" Prevent window death
""""""""""""""""""""""
nmap <C-w>o :echoerr "Go fuck yourself :-)"<CR>
nmap <C-w><C-o> :echoerr "Go fuck yourself :-)"<CR>

" Fugitive
""""""""""
nmap <LocalLeader>gg :Gstatus<CR>
nmap <LocalLeader>gl :Glog --<CR>
nmap <LocalLeader>gL :Glog -- %
nmap <LocalLeader>gr :Ggrep<SPACE>
nmap <LocalLeader>gb :Gblame<CR>
nmap <LocalLeader>gB :Gbrowse!<CR>
nmap <LocalLeader>ge :Gedit<CR>
nmap <LocalLeader>gw :Gwrite<CR>
nmap <LocalLeader>gR :Gread<CR>
nmap <LocalLeader>gd :Gdiff<CR>
nmap <LocalLeader>gc :Gcommit<CR>

" vim-easy-align
""""""""""""""""
vmap <Enter> <Plug>(EasyAlign)


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
"let NERDTreeDirArrows = 0
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
let g:toggle_list_no_mappings = 1       " disable default plugin bindings
let g:toggle_list_copen_command='Copen' " use Dispatch Copen command

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" Fugitive
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
set statusline=%<%f\ %h%m%r%{fugitive#statusline()}%=%-14.(%l,%c%V%)\ %P
let g:fugitive_github_domains = ['github.com', 'github.emcrubicon.com']

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" Unimpaired
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" make :cnext and :cprevious easier on my keymap
map ]j :cnext<CR>
map ]k :cprevious<CR>

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" Gist
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:gist_post_private = 1
let g:gist_show_privates = 1
let g:gist_open_browser_after_post = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" YouCompleteMe
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
" Don't use <tab> to cycle thru PUM, use ^n and ^p as we're used to doing.
" This also solves problems with using UltiSnips simultaneously, which uses
" <tab> as its default trigger.
let g:ycm_key_list_select_completion=['<C-n>']
let g:ycm_key_list_previous_completion=['<C-p>']

" read tags file
let g:ycm_collect_identifiers_from_tags_files = 1

"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
""" UltiSnips
"""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""""
let g:UltiSnipsExpandTrigger="<Tab>"
let g:UltiSnipsJumpForwardTrigger="<Tab>"
let g:UltiSnipsJumpBackwardTrigger="<S-Tab>"

" If you want :UltiSnipsEdit to split your window.
let g:UltiSnipsEditSplit="vertical"

setlocal sw=2 sts=2 si
let ruby_fold=1

"
" infrastructure
"
map <buffer> <F10> :w<CR>:!clear; date; ruby -wc <C-r>%<CR>
map <buffer> <F11> :w<CR>:!clear; date; ruby <C-r>%<CR>
map <buffer> <S-F11> :w<CR>:!clear; date; ruby <C-r>%

map <buffer> <LocalLeader>ubr i#!/usr/bin/ruby<CR><BS><CR>
imap <buffer> <LocalLeader>ubr #!/usr/bin/ruby<CR><BS>

nnoremap <silent> <buffer> <cr> :RubySearch<cr>

"
" multi line stuff
"
map <buffer> <LocalLeader>beg ibegin<CR>rescue => e<CR>end<ESC>kO<TAB>
imap <buffer> <LocalLeader>beg begin<CR>rescue => e<CR>end<ESC>kO<TAB>

map <buffer> <LocalLeader>kl iclass<CR>end #class <ESC>kA 
imap <buffer> <LocalLeader>kl class<CR>end #class <ESC>kA 

map <buffer> <LocalLeader>def idef<CR>end<ESC>kA 
imap <buffer> <LocalLeader>def def<CR>end<ESC>kA 

map <buffer> <LocalLeader>di idef initialize()<CR>end<ESC>k$i
imap <buffer> <LocalLeader>di def initialize()<CR>end<ESC>k$i

map <buffer> <LocalLeader>do ido \|\|<CR>end<ESC>k$i
imap <buffer> <LocalLeader>do do \|\|<CR>end<ESC>k$i

" imap <buffer> .eb .each { \|\| }<ESC>hhi

" imap <buffer> .ed .each do \|\|<CR>end<ESC>k$i

map <buffer> <LocalLeader>if iif<CR><BS>end<ESC>kA 
imap <buffer> <LocalLeader>if if<CR><BS>end<ESC>kA 

map <buffer> <LocalLeader>mod imodule<CR>end #module <ESC>kA 
imap <buffer> <LocalLeader>mod module<CR>end #module <ESC>kA 

map <buffer> <LocalLeader>un iunless<CR>end<ESC>kA 
imap <buffer> <LocalLeader>un unless<CR>end<ESC>kA 

"
" one liners
"
imap <buffer> <LocalLeader>aa attr_accessor :
imap <buffer> <LocalLeader>ar attr_reader :
imap <buffer> <LocalLeader>aw attr_writer :

" map <buffer> .b i\<ESC>
" imap <buffer> .b \

map <buffer> <LocalLeader>br i#{}<ESC>i
imap <buffer> <LocalLeader>br #{}<ESC>i

map <buffer> <LocalLeader>c i@<ESC>
imap <buffer> <LocalLeader>c @

map <buffer> <LocalLeader>e0 iexit(0)<ESC>
imap <buffer> <LocalLeader>e0 exit(0)

map <buffer> <LocalLeader>e1 iexit(1)<ESC>
imap <buffer> <LocalLeader>e1 exit(1)

map <buffer> <LocalLeader>env iENV['']<ESC>hi
imap <buffer> <LocalLeader>env ENV['']<ESC>hi

imap <buffer> <LocalLeader>l <ESC>A

imap <buffer> <LocalLeader>pn puts ""<ESC>i
imap <buffer> <LocalLeader>pr print 

map <buffer> <LocalLeader>r i#<ESC>
imap <buffer> <LocalLeader>r #

" map <buffer> .r i"<ESC>
" imap <buffer> .r "

map <buffer> <LocalLeader>rq irequire ''<ESC>i
imap <buffer> <LocalLeader>rq require ''<ESC>i

map <buffer> <LocalLeader>t i=~<ESC>
imap <buffer> <LocalLeader>t =~ 

"
" Chef shit
"
" map <buffer> ..deb iChef::Log.debug("")<ESC>hi
" imap <buffer> ..deb Chef::Log.debug("")<ESC>hi

map <buffer> <LocalLeader>nd inode['']<ESC>hi
imap <buffer> <LocalLeader>nd node['']<ESC>hi

"
" ERB shit
"
map <buffer> <LocalLeader>er i<%  %><ESC>3ha
imap <buffer> <LocalLeader>er <%  %><ESC>3ha

map <buffer> <LocalLeader>eq i<%  -%><ESC>4ha
imap <buffer> <LocalLeader>eq <%  -%><ESC>4ha

map <buffer> <LocalLeader>ev i<%=  %><ESC>3ha
imap <buffer> <LocalLeader>ev <%=  %><ESC>3ha

"
" RSpec shit
"
map <buffer> <LocalLeader>it iit '' do<CR>end<ESC>k$2ba
imap <buffer> <LocalLeader>it it '' do<CR>end<ESC>k$2ba


setlocal smartindent
setlocal formatoptions-=q
setlocal keywordprg=perldoc\ -f

inoremap # y<BS>#

map <buffer> <F11> :w<CR>:!clear; date; perl <C-r>%<CR>
map <buffer> <S-F11> :w<CR>:!clear; date; perl <C-r>% 
map <buffer> <F10> :w<CR>:!clear; date; perl -wc <C-r>%<CR>

imap <buffer> <LocalLeader>l <ESC>A

map <buffer> <LocalLeader>ubp i#!/usr/bin/perl<CR><BS><CR>
imap <buffer> <LocalLeader>ubp #!/usr/bin/perl<CR><BS>

map <buffer> <LocalLeader>ubw i#!/usr/bin/perl -w<CR><BS><CR>
imap <buffer> <LocalLeader>ubw #!/usr/bin/perl -w<CR><BS>

map <buffer> <LocalLeader>beg 0iBEGIN<CR>{<CR>}<ESC>O
imap <buffer> <LocalLeader>beg <ESC>0iBEGIN<CR>{<CR>}<ESC>O

" map <buffer> .c i$<ESC>
" imap <buffer> .c $
map <buffer> <LocalLeader>c i@<ESC>
imap <buffer> <LocalLeader>c @
map <buffer> <LocalLeader>h i%<ESC>
imap <buffer> <LocalLeader>h %
map <buffer> <LocalLeader>r i#<ESC>
imap <buffer> <LocalLeader>r #

map <buffer> <LocalLeader>nt i!<ESC>
imap <buffer> <LocalLeader>nt !

map <buffer> <LocalLeader>t i=~<ESC>
imap <buffer> <LocalLeader>t =~ 

" map <buffer> .n i\n<ESC>
" imap <buffer> .n \n

" map <buffer> .r i"<ESC>
" imap <buffer> .r "

" map <buffer> .- i$_<ESC> 
" imap <buffer> .- $_

map <buffer> <LocalLeader>- i@_<ESC> 
imap <buffer> <LocalLeader>- @_

map <buffer> <LocalLeader>err i$!<ESC>
imap <buffer> <LocalLeader>err $!

map <buffer> <LocalLeader>ev ieval<ESC>
imap <buffer> <LocalLeader>ev eval

map <buffer> <LocalLeader>evb ieval {<CR><CR>};<ESC>O
imap <buffer> <LocalLeader>evb eval {<CR><CR>};<ESC>O

map <buffer> <LocalLeader>br $a {<CR><CR>}<ESC>O
imap <buffer> <LocalLeader>br <ESC>$a {<CR><CR>}<ESC>O
imap <buffer> <LocalLeader>ebr { <CR><CR>}<ESC>O
map <buffer> <LocalLeader>ebr a{ <CR><CR>}<ESC>O

map <buffer> <LocalLeader>par Bi(<ESC>Ea)<ESC>
imap <buffer> <LocalLeader>par <ESC>Bi(<ESC>Ea)

map <buffer> <LocalLeader><LocalLeader>br 0/ if <CR>xi<CR><ESC>kddpk$xa {<CR><ESC>j>>o}<ESC>k$a;<ESC>:nohl<CR>
imap <buffer> <LocalLeader><LocalLeader>br <ESC>0/ if <CR>xi<CR><ESC>kddpk$xa {<CR><ESC>j>>o}<ESC>k$a;<ESC>:nohl<CR>

map <buffer> <LocalLeader><LocalLeader>or 0/ or <CR>xi<CR><TAB><ESC>$:nohl<CR>
imap <buffer> <LocalLeader><LocalLeader>or <ESC>0/ or <CR>xi<CR><TAB><ESC>$:nohl<CR>

map <buffer> <LocalLeader>if iif ()<ESC>i
map <buffer> <LocalLeader>iif ^iif (<ESC>$a)<ESC>
imap <buffer> <LocalLeader>if if ()<ESC>i

map <buffer> <LocalLeader>ei oelsif ()<ESC>i
imap <buffer> <LocalLeader>ei elsif ()<ESC>i

map <buffer> <LocalLeader>el oelse<LocalLeader>br
imap <buffer> <LocalLeader>el else<LocalLeader>br

map <buffer> <LocalLeader>un iunless ()<ESC>i
imap <buffer> <LocalLeader>un unless ()<ESC>i

map <buffer> <LocalLeader>wh iwhile ()<ESC>i
imap <buffer> <LocalLeader>wh while ()<ESC>i

map <buffer> <LocalLeader>for ifor ()<ESC>i
imap <buffer> <LocalLeader>for for ()<ESC>i

map <buffer> <LocalLeader>fe iforeach ()<ESC>i
imap <buffer> <LocalLeader>fe foreach ()<ESC>i

" map <buffer> .b i\<ESC>
" imap <buffer> .b \

map <buffer> <LocalLeader>qm iquotemeta <ESC>
imap <buffer> <LocalLeader>qm quotemeta 

map <buffer> <LocalLeader>ang i<><ESC>
imap <buffer> <LocalLeader>ang <><ESC>i

map <buffer> <LocalLeader>pr iprint "";<ESC>hi
map <buffer> <LocalLeader>pn iprint "\n";<ESC>3ba
imap <buffer> <LocalLeader>pr print "";<ESC>hi
imap <buffer> <LocalLeader>pn print "\n";<ESC>3ba

map <buffer> <LocalLeader>ph iprint "\n";<ESC>5hi 
imap <buffer> <LocalLeader>ph print "\n";<ESC>5hi 

map <buffer> <LocalLeader>pd iprint "[d] \n" if ($debug);<ESC>7bi
imap <buffer> <LocalLeader>pd print "[d] \n" if ($debug);<ESC>7bi

map <buffer> <LocalLeader>ret i$? >> 8 > 0<ESC>
imap <buffer> <LocalLeader>ret $? >> 8 > 0

map <buffer> <LocalLeader>di idie "\n";<ESC>3ba
imap <buffer> <LocalLeader>di die "\n";<ESC>3ba

map <buffer> <LocalLeader>ba ibail("");<ESC>2hi
imap <buffer> <LocalLeader>ba bail("");<ESC>2hi

map <buffer> <LocalLeader>bp iboldprint("[-] ");<ESC>2hi
imap <buffer> <LocalLeader>bp boldprint("[-] ");<ESC>2hi

map <buffer> <LocalLeader>bw iboldwarn("[w] \n");<ESC>4hi
imap <buffer> <LocalLeader>bw boldwarn("[w] \n");<ESC>4hi

map <buffer> <LocalLeader>war iwarn "\n";<ESC>3ba
imap <buffer> <LocalLeader>war warn "\n";<ESC>3ba

map <buffer> <LocalLeader>def idefined <ESC>
imap <buffer> <LocalLeader>def defined 

map <buffer> <LocalLeader>nd i!defined <ESC>
imap <buffer> <LocalLeader>nd !defined 

map <buffer> <LocalLeader>arg i@ARGV
imap <buffer> <LocalLeader>arg @ARGV

" map <buffer> .arg i$ARGV[]<ESC>i
" imap <buffer> .arg $ARGV[]<ESC>i

map <buffer> <LocalLeader>env i$ENV{''}<ESC>hi
imap <buffer> <LocalLeader>env $ENV{''}<ESC>hi

map <buffer> <LocalLeader>sig i$SIG{''}<ESC>hi
imap <buffer> <LocalLeader>sig $SIG{''}<ESC>hi

map <buffer> <LocalLeader>std iSTDIN<ESC>
imap <buffer> <LocalLeader>std STDIN

map <buffer> <LocalLeader>sca iscalar <ESC>
imap <buffer> <LocalLeader>sca scalar 

map <buffer> <LocalLeader>od iopendir()<ESC>i
imap <buffer> <LocalLeader>od opendir()<ESC>i

map <buffer> <LocalLeader>sys isystem ("");<ESC>2hi
imap <buffer> <LocalLeader>sys system ("");<ESC>2hi

map <buffer> <LocalLeader>sub isub <CR>{<CR>}<ESC>2kA
imap <buffer> <LocalLeader>sub sub <CR>{<CR>}<ESC>2kA

map <buffer> <LocalLeader>e0 iexit 0;<ESC>
imap <buffer> <LocalLeader>e0 exit 0;<ESC>

map <buffer> <LocalLeader>e1 iexit 1;<ESC>
imap <buffer> <LocalLeader>e1 exit 1;<ESC>

setlocal smartindent
setlocal formatoptions-=q

map <buffer> <LocalLeader>php i<?php<CR>?><ESC>O<TAB>
imap <buffer> <LocalLeader>php <?php<CR>?><ESC>O<TAB>

map <buffer> <LocalLeader>r i//<ESC>
imap <buffer> <LocalLeader>r // 

map <buffer> <LocalLeader>cb }{wO<BS>/*<ESC>}bo<BS>*/<ESC>
map <buffer> <LocalLeader>co i/*  */<ESC>bh
imap <buffer> <LocalLeader>co /*  */<ESC>bhi

map <buffer> .c i$<ESC>
imap <buffer> .c $

map <buffer> .n i\n<ESC>
imap <buffer> .n \n

map <buffer> .r i"<ESC>
imap <buffer> .r "

map <buffer> <LocalLeader>if iif ()<ESC>i
map <buffer> <LocalLeader>cif ^iif (<ESC>$a)<ESC>
imap <buffer> <LocalLeader>if if ()<ESC>i

map <buffer> <LocalLeader>ei else <LocalLeader>if
imap <buffer> <LocalLeader>ei oelse <LocalLeader>if

map <buffer> <LocalLeader>el oelse<CR>
imap <buffer> <LocalLeader>el else<CR>

map <buffer> <LocalLeader>br $a {<CR><CR>}<ESC>O
imap <buffer> <LocalLeader>br <ESC>$a {<CR><CR>}<ESC>O
imap <buffer> <LocalLeader>ebr { <CR><CR>}<ESC>O
map <buffer> <LocalLeader>ebr a{ <CR><CR>}<ESC>O

map <buffer> <LocalLeader>par Bi(<ESC>Ea)<ESC>
imap <buffer> <LocalLeader>par <ESC>Bi(<ESC>Ea)

map <buffer> <LocalLeader>ech iecho "";<ESC>hi
imap <buffer> <LocalLeader>ech echo "";<ESC>hi

map <buffer> <LocalLeader>pn iprint "\n";<ESC>3ba
imap <buffer> <LocalLeader>pn print "\n";<ESC>3ba

map <buffer> <LocalLeader>pr iprint_r ();<ESC>hi
imap <buffer> <LocalLeader>pr print_r ();<ESC>hi

imap <buffer> <LocalLeader>cmp strcasecmp()<ESC>i

map <buffer> <LocalLeader>hcb }{wO<BS><!--<ESC>}bo<BS>--><ESC>
imap <buffer> <LocalLeader>hc <!--  --><ESC>bhi

map <buffer> <LocalLeader>ht i<HTML><CR><TAB><HEAD><CR><TAB><TITLE></TITLE><CR><BS></HEAD><CR><CR><BODY BGCOLOR=''><CR></BODY><CR><BS></HTML><ESC>3k$2ba

imap <buffer> <LocalLeader>css <LINK REL='StyleSheet' TYPE='text/css' HREF=''><ESC>hi
map <buffer> <LocalLeader>css i<LINK REL='StyleSheet' TYPE='text/css' HREF=''><ESC>hi

map <buffer> <LocalLeader>bb i<BR><ESC>
imap <buffer> <LocalLeader>bb <BR>

map <buffer> <LocalLeader>pa i<P><ESC>
imap <buffer> <LocalLeader>pa <P>

imap <buffer> <LocalLeader>ah <A HREF=''></A><ESC>2bla

imap <buffer> <LocalLeader>sp <SPAN CLASS='' STYLE=''></SPAN><ESC>4bla
imap <buffer> <LocalLeader>ss <SPAN STYLE=''></SPAN><ESC>2bla
imap <buffer> <LocalLeader>sc <SPAN CLASS=''></SPAN><ESC>2bla

imap <buffer> <LocalLeader>tab <TABLE BORER=0><TR><TD></TD></TR></TABLE><ESC>6ba

imap <buffer> <LocalLeader>h1 <H1></H1><ESC>2ba
imap <buffer> <LocalLeader>h2 <H2></H2><ESC>2ba
imap <buffer> <LocalLeader>h3 <H3></H3><ESC>2ba
imap <buffer> <LocalLeader>h4 <H4></H4><ESC>2ba
imap <buffer> <LocalLeader>h5 <H5></H5><ESC>2ba
imap <buffer> <LocalLeader>h6 <H6></H6><ESC>2ba

map <buffer> <LocalLeader>im i<IMG SRC=""><ESC>hi
imap <buffer> <LocalLeader>im <IMG SRC=""><ESC>hi

imap <buffer> <LocalLeader>gt &gt;
imap <buffer> <LocalLeader>lt &lt;
imap <buffer> <LocalLeader>nb &nbsp;
map <buffer> <LocalLeader>nb i&nbsp;<ESC>l

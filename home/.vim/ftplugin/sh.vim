" enable function, heredoc, if/else/do folding
let g:sh_fold_enabled = 7

map <buffer> <LocalLeader>bin i#!/bin/sh<CR><BS><CR><CR>
imap <buffer> <LocalLeader>bin #!/bin/sh<CR><BS><CR><CR>

map <buffer> .c i$<ESC>
imap <buffer> .c $

map <buffer> <LocalLeader>r i#<ESC>
imap <buffer> <LocalLeader>r # 

map <buffer> .n i\n<ESC>
imap <buffer> .n \n

map <buffer> .r i"<ESC>
imap <buffer> .r "

map <buffer> .b i\<ESC>
imap <buffer> .b \

imap <buffer> <LocalLeader>l <ESC>A

map <buffer> <LocalLeader>par Bi(<ESC>Ea)<ESC>
imap <buffer> <LocalLeader>par <ESC>Bi(<ESC>Ea)

map <buffer> <LocalLeader>if iif ; then<CR>fi<ESC>k$2bi
imap <buffer> <LocalLeader>if if ; then<CR>fi<ESC>k$2bi

map <buffer> <LocalLeader>t i[  ]<ESC>hi
imap <buffer> <LocalLeader>t [  ]<ESC>hi

map <buffer> .t i[[  ]]<ESC>2hi
imap <buffer> .t [[  ]]<ESC>2hi

map <buffer> <LocalLeader>ech iecho ""<ESC>i
imap <buffer> <LocalLeader>ech echo ""<ESC>i

map <buffer> <LocalLeader>func ifunction () {<CR>}<ESC>k$bi
imap <buffer> <LocalLeader>func function () {<CR>}<ESC>k$bi

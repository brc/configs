imap <buffer> <LocalLeader>l <ESC>A

map <buffer> <LocalLeader>br i{{  }}<ESC>3ha
imap <buffer> <LocalLeader>br {{  }}<ESC>3ha

map <buffer> .r i"<ESC>
imap <buffer> .r "

imap <buffer> <LocalLeader>n <ESC>0i- name:<SPACE>
map <buffer> <LocalLeader>n 0i- name:<SPACE>

imap <buffer> <LocalLeader>it with_items:<SPACE>
map <buffer> <LocalLeader>it i<TAB>with_items:<SPACE>

imap <buffer> <LocalLeader>w when:<SPACE>
" can't add ,w mapping for cmd mode because conflict with custom ^W mapping

imap <buffer> <LocalLeader>in - include:<SPACE>
map <buffer> <LocalLeader>in i- include:<SPACE>

imap <buffer> ..in - { include: .yml, }<ESC>3bi
map <buffer> ..in i- { include: .yml, }<ESC>3bi

map <buffer> <F10> :!emc-reposync.sh<CR>

imap <buffer> <LocalLeader>hv hostvars

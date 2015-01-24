source ~/.vim/ftplugin/jinja.vim

"
" attributes/requisites
"
map <buffer> <LocalLeader>rq i- require:<CR>- pkg:<SPACE>
imap <buffer> <LocalLeader>rq - require:<CR>- pkg:<SPACE>

map <buffer> <LocalLeader>wa i- watch:<CR>- file:<SPACE>
imap <buffer> <LocalLeader>wa - watch:<CR>- file:<SPACE>

map <buffer> <LocalLeader>wi i- watch_in:<CR>- file:<SPACE>
imap <buffer> <LocalLeader>wi - watch_in:<CR>- file:<SPACE>

imap <buffer> <LocalLeader>nm - name:<SPACE>

imap <buffer> <LocalLeader>env - env:<CR>-<SPACE>

"
" File states
"
map <buffer> <LocalLeader>fm i:<CR>file.managed:<CR>- source: salt://<CR>- template: jinja<ESC>3kI
imap <buffer> <LocalLeader>fm :<CR>file.managed:<CR>- source: salt://<CR>- template: jinja<ESC>3kI

map <buffer> <LocalLeader>fa i:<CR>file.absent<ESC>kI
imap <buffer> <LocalLeader>fa :<CR>file.absent<ESC>kI

imap <buffer> <LocalLeader>fs :<CR>file.symlink:<CR>- target:<ESC>2kI

"
" Package states
"
map <buffer> <LocalLeader>pi i:<CR>pkg.installed<ESC>kI
imap <buffer> <LocalLeader>pi :<CR>pkg.installed<ESC>kI

map <buffer> <LocalLeader>pr i:<CR>pkg.removed<ESC>kI
imap <buffer> <LocalLeader>pr :<CR>pkg.removed<ESC>kI

"
" Service states
"
map <buffer> <LocalLeader>sr i:<CR>service.running:<CR>- enable: True<ESC>2kI
imap <buffer> <LocalLeader>sr :<CR>service.running:<CR>- enable: True<ESC>2kI

map <buffer> <LocalLeader>sd i:<CR>service.dead:<CR>- enable: False<ESC>2kI
imap <buffer> <LocalLeader>sd :<CR>service.dead:<CR>- enable: False<ESC>2kI

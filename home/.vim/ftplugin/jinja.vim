imap <buffer> <LocalLeader>br {{}}<ESC>hi

map <buffer> <LocalLeader>r i{#  -#}<ESC>3hi
imap <buffer> <LocalLeader>r {#  -#}<ESC>3hi

map <buffer> <LocalLeader>cb i{#-<CR> #<CR>#}<ESC>ka<SPACE>
imap <buffer> <LocalLeader>cb {#-<CR> #<CR>#}<ESC>ka<SPACE>

map <buffer> <LocalLeader>set i{% set  = -%}<ESC>2bhi
imap <buffer> <LocalLeader>set {% set  = -%}<ESC>2bhi

map <buffer> <LocalLeader>do i{% do  -%}<ESC>bhi
imap <buffer> <LocalLeader>do {% do  -%}<ESC>bhi

map <buffer> <LocalLeader>if i{% if  -%}<CR>{% endif %}<ESC>k$3hi
imap <buffer> <LocalLeader>if {% if  -%}<CR>{% endif %}<ESC>k$3hi

map <buffer> <LocalLeader>ei i{% elif  -%}<ESC>3hi
imap <buffer> <LocalLeader>ei {% elif  -%}<ESC>3hi

map <buffer> <LocalLeader>el i{% else -%}<CR>
imap <buffer> <LocalLeader>el {% else -%}<CR>

map <buffer> <LocalLeader>for i{% for x in  -%}<CR>{% endfor %}<ESC>k$3hi
imap <buffer> <LocalLeader>for {% for x in  -%}<CR>{% endfor %}<ESC>k$3hi

map <buffer> <LocalLeader>mac i{% macro () -%}<CR>{% endmacro %}<ESC>k$bbi
imap <buffer> <LocalLeader>mac {% macro () -%}<CR>{% endmacro %}<ESC>k$bbi

map <buffer> <LocalLeader>im i{% from '.jinja' import foo with context -%}<ESC>6Ba
imap <buffer> <LocalLeader>im {% from '.jinja' import foo with context -%}<ESC>6Ba


setlocal smartindent

map <buffer> <LocalLeader>inc i#include <stdio.h>#include <>i
imap <buffer> <LocalLeader>inc o#include <>i

map <buffer> <LocalLeader>vm ovoid main (void){}O
imap <buffer> <LocalLeader>vm void main (void){}O

map <buffer> <LocalLeader>im oint main (int argc, char *argv[]){}O
imap <buffer> <LocalLeader>im int main (int argc, char *argv[]){}O

imap <buffer> <LocalLeader>co /*  */hhi
map <buffer> <LocalLeader>co i/*  */hhi

imap <buffer> <LocalLeader>pf printf("");2hi

map <buffer> <F5> :!clear; ./`basename % .c`
map <buffer> <F7> :w:!clear; gcc -Wall -o `basename % .c` %

imap <buffer> <LocalLeader>br {}O
map <buffer> <LocalLeader>br $bo{}O

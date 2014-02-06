#
# ~/.bashrc
#

# If not running interactively, don't do anything
[[ $- != *i* ]] && return

# make a smarter shell
shopt -s nocaseglob
shopt -s extglob
shopt -s checkwinsize

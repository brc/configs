# vim: set tags=~/.zsh/tags :

# It seems like any command aliased to 'nocorrect <command>' won't work
# properly with run-help.  run-help just reports:
#
#    mkdir is an alias for nocorrect mkdir
#    (Press any key for more help or q to quit)
#

# Confusingly, i'm aliasing run-help to 'nocorrect run-help' so
# it doesn't ask me this kind of shit when i invoke it:
#
#   /git/invsblduck/configs> run-help git                                          (master *% u=)
#   zsh: correct 'git' to '.git' [nyae]?
#
alias run-help='nocorrect run-help'

#alias sudo='nocorrect sudo'    # interferes with run-help-sudo
alias man='nocorrect man'
alias mv='nocorrect mv'
alias mysql='nocorrect mysql'
alias mkdir='nocorrect mkdir'
alias gist='nocorrect gist'

setopt correct_all

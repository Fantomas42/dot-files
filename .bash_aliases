alias l='ls -l'
alias la='ls -la'
alias emacs='emacs -nw'
alias e='emacs'
alias j='jobs'
alias clean='find . -maxdepth 1 -type f \( -name "*.pyc" -o -name "\#*" -o -name "*~" \) -exec rm -fv {} \;'
alias cleanr='find . -type f \( -name "*.pyc" -o -name "\#*" -o -name "*~" \) -exec rm -fv {} \;'
alias xview='xdg-open .'

alias nas='ssh admin@192.168.0.11'
alias gozinnia='cd /home/fantomas/dev/django-blog-zinnia/ && source ./bin/activate && git st;'

PS1="\[\e[3;34m\][\[\e[m\]\# \[\e[0;33m\]\t\[\e[m\]\[\e[3;34m\]]\[\e[m\]\[\e[0;31m\]Fantomas:\[\e[0;32m\]\w$ \[\e[m\]"

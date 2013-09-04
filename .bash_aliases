alias l='ls -l'
alias la='ls -la'
alias emacs='emacs -nw'
alias e='emacs'
alias j='jobs'
alias clean='find . -maxdepth 1 -type f \( -name "*.pyc" -o -name "\#*" -o -name "*~" \) -exec rm -fv {} \;'
alias cleanr='find . -type f \( -name "*.pyc" -o -name "\#*" -o -name "*~" \) -exec rm -fv {} \;'
alias xview='xdg-open .'

if [ -f ~/.bash_aliases_local ]; then
    . ~/.bash_aliases_local
fi

if [ -f ~/.bash_prompt ]; then
    . ~/.bash_prompt
fi

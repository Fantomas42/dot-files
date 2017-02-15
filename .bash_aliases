# Load custom env
. ~/.bash_env

alias l='ls -l'
alias la='ls -la'
alias emacs='emacsclient -t'
alias e='emacs'
alias j='jobs'
alias git='LANGUAGE=en_US.UTF-8 git'
alias clean='find . -maxdepth 1 -type f \( -name "*.pyc" -o -name "*.pyo" -o -name "\#*" -o -name "*~" \) -exec rm -fv {} \;'
alias cleanr='find . -type f \( -name "*.pyc" -o -name "*.pyo" -o -name "\#*" -o -name "*~" \) -exec rm -fv {} \;'
alias xview='xdg-open .'
alias hostedit='sudo emacs -nw /etc/hosts'

alias pbcopy='xclip -selection clipboard'
alias pbpaste='xclip -selection clipboard -o'

if [ -f ~/.bash_aliases_local ]; then
    . ~/.bash_aliases_local
fi


# Load custom prompt
. ~/.bash_prompt
# Load virtualenv autoactivate
. ~/.bash_venv_activate

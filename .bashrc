#-----------------------------------------------------------------------------
#   _               _
#  | |__   __ _ ___| |__    _ __ ___
#  | '_ \ / _` / __| '_ \  | '__/ __|
#  | |_) | (_| \__ \ | | | | | | (__
#  |_.__/ \__,_|___/_| |_| |_|  \___|
#
#-----------------------------------------------------------------------------
. $HOME/.bash.d/git-prompt.sh

if ! shopt -oq posix; then
    if [ -f /usr/share/bash-completion/bash_completion ]; then
        . /usr/share/bash-completion/bash_completion
    elif [ -f /etc/bash_completion ]; then
        . /etc/bash_completion
    fi
fi

#-----------------------------------------------------------------------------
# Bash Colors (9):
# a: black,   b: red,  c: green,      d: brown,  e: blue,
# f: magenta, g: cyan, h: light gray, x: default
#
# Order (11):
# DIR, SYM_LINK, SOCKET, PIPE, EXE, BLOCK_SP, CHAR_SP, EXE_SUID, EXE_GUID,
# DIR_STICKY, DIR_WO_STICKY
#
# Format String (11): (Xx)
# export LSCOLORS="XxXxXxXxXxXxXxXxXxXxXx"
#
export LSCOLORS="GxGxxxxxCxxxxxxxxxxxxx"
export LSOPTIONS="--color=auto"
export CLICOLOR="Yes"

function git_string() {
    local blue='\[\e[0;34m\]'

    if [ $(__git_ps1 %s) ]; then
        echo "${blue}git:$(__git_ps1 %s)"
    else
        echo ""
    fi
}

function compose_prompt() {
    local fg_light_theme='\[\e[0;30m\]'
    local fg_dark_theme='\[\e[0m\]'

    local neutral=$fg_dark_theme
    local red='\[\e[0;31m\]'
    local green='\[\e[0;32m\]'
    local purple='\[\e[0;35m\]'

    local open="${neutral}["
    local close="${neutral}]"
    local separator="${neutral}|"

    local machine="${purple}\u@\h"
    local path="${green}\w"
    local git=$(git_string)

    PS1="${open}${machine}${separator}${path}${separator}${git}${close}\n↳ "
}

PROMPT_COMMAND=compose_prompt

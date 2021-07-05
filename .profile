#-------------------------------------------------------------------------------
#  _               _                        __ _ _
# | |__   __ _ ___| |__    _ __  _ __ ___  / _(_) | ___
# | '_ \ / _` / __| '_ \  | '_ \| '__/ _ \| |_| | |/ _ \
# | |_) | (_| \__ \ | | | | |_) | | | (_) |  _| | |  __/
# |_.__/ \__,_|___/_| |_| | .__/|_|  \___/|_| |_|_|\___|
#                         |_|
#
#-------------------------------------------------------------------------------
# Banner text generated with figlet.
#
# ~/.bash_profile
#        The personal initialization file, executed for login shells
# ~/.bashrc
#        The individual per-interactive-shell startup file
# ~/.bash_logout
#        The individual login shell cleanup file, executed when a login shell exits
# ~/.inputrc
#        Individual readline initialization file
#
# This file is loaded at login.
# It can be aliased as: .bash_profile, .login
# For simplicity, ensure there is no separate .bash_profile or other
# shell-specific login file. (Some folks alias this file as those ones.)
#
# This file should set shell-independent things like env vars.
#-------------------------------------------------------------------------------

#-------------------------------------------------------------------------------
# Aliases
#-------------------------------------------------------------------------------
alias rm='rm -i'
alias bc='bc -l'

alias grep='grep --line-buffered'
alias dirs='dirs -v'

#-------------------------------------------------------------------------------
# Env vars and PATH
#-------------------------------------------------------------------------------
export EDITOR="ng"
export SSH_ENV="$HOME/.ssh/env"

[ -d "$HOME/go" ] && export GOPATH="$HOME/go"
[ -d "$GOPATH/bin" ] && export PATH="$GOPATH/bin:$PATH"
[ -d "$HOME/.local/bin" ] && export PATH="$HOME/.local/bin:$PATH"
[ -f "$HOME/.local/env" ] && . $HOME/.local/env

export CODE_PATH="$HOME/code"

#-------------------------------------------------------------------------------
# OS-specific configuration and environment-specific customization
#-------------------------------------------------------------------------------
# eg.,: .profile-linux, .profile-darwin
. $HOME/.profile-$(uname | tr '[:upper:]' '[:lower:]')
[ -f "$HOME/.profile-local" ] && . $HOME/.profile-local

. $HOME/.bashrc

# De-duplicate elements in $PATH
if [ -n "$PATH" ]; then
    old_PATH=$PATH:; PATH=
    while [ -n "$old_PATH" ]; do
        x=${old_PATH%%:*}       # the first remaining entry
        case $PATH: in
            *:"$x":*) ;;          # already there
            *) PATH=$PATH:$x;;    # not there yet
        esac
        old_PATH=${old_PATH#*:}
    done
    PATH=${PATH#:}
    unset old_PATH x
fi

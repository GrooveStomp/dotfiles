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
# Env vars and PATH
#-------------------------------------------------------------------------------
export EDITOR="emacs -nw"
export SSH_ENV="$HOME/.ssh/env"
export GOPATH="$HOME/go"
export PATH="$GOPATH/bin:$PATH"

[ -d "$HOME/bin" ]     && export PATH="$HOME/bin:$PATH"
[ -d "/usr/local/go" ] && export PATH="/usr/local/go/bin:$PATH"
[ -d "$HOME/.cargo/bin" ] && export PATH="$HOME/.cargo/bin:$PATH"
[ -f "$HOME/.bash.d/local-env-vars.sh" ] && . "$HOME/.bash.d/local-env-vars.sh"
[ -d "/usr/local/opt/llvm/bin" ] && export PATH="/usr/local/opt/llvm/bin:$PATH"

#-------------------------------------------------------------------------------
# Aliases
#-------------------------------------------------------------------------------
alias ls='ls -1'
alias lld='ls -Alhd'
alias ll='ls -Alh'
alias la='ls -A'
alias l='ls -CF'
alias rm='rm -i'
alias bc='bc -l'

alias em="emacs -nw -q"
alias vi="emacs -nw -q"
alias vim="emacs -nw -q"

alias grep='grep --line-buffered'
alias dirs='dirs -v'

. ~/code/dotfiles/.profile-$(uname | tr '[:upper:]' '[:lower:]')

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

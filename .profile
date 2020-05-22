#-------------------------------------------------------------------------------
#   _               _                        __ _ _
#  | |__   __ _ ___| |__    _ __  _ __ ___  / _(_) | ___
#  | '_ \ / _` / __| '_ \  | '_ \| '__/ _ \| |_| | |/ _ \
#  | |_) | (_| \__ \ | | | | |_) | | | (_) |  _| | |  __/
#  |_.__/ \__,_|___/_| |_| | .__/|_|  \___/|_| |_|_|\___|
#                          |_|
#
#-------------------------------------------------------------------------------
# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

#-------------------------------------------------------------------------------
# Env vars and path
#-------------------------------------------------------------------------------
export EDITOR="emacs -nw"
export SSH_ENV="$HOME/.ssh/env"
export GOPATH="$HOME/code/go"
export PATH="$HOME/bin:$PATH"

[ -d "$HOME/.local/bin" ] && export PATH="$HOME/.local/bin:$PATH"
[ -d "/snap/bin" ] && export PATH="/snap/bin:$PATH"
[ -d "$HOME/.cargo/bin" ] && export PATH="$HOME/.cargo/bin:$PATH"
[ -d "$HOME/bin" ] && export PATH="$HOME/bin:$PATH"
[ -f "$HOME/bash.d/local-env-vars.sh" ] && . "$HOME/.bash.d/local-env-vars.sh"
[ -d "$HOME/code/go/bin" ] && export PATH="$HOME/code/go/bin:$PATH"

#-------------------------------------------------------------------------------
# Aliases
#-------------------------------------------------------------------------------
alias bc="bc -l"
alias rm="rm -i"
alias vim="emacs -nw -q"
alias vi="emacs -nw -q"
alias grep="grep --line-buffered"
alias dirs="dirs -v"


# if running bash
if [ $(basename $SHELL) == "bash" ]; then
	  . "$HOME/.bashrc"
fi

export PATH="$HOME/.cargo/bin:$PATH"

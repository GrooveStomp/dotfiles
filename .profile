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
# This file is loaded at login.
# For simplicity, ensure there is no separate .bash_profile or other
# shell-specific login file. (Some folks alias this file as those ones.)
#
# This file should set shell-independent things like env vars.
#

#-------------------------------------------------------------------------------
# Env vars and PATH
#-------------------------------------------------------------------------------
export EDITOR="NO_EMACS_THEME=1 emacs -nw"
export SSH_ENV="$HOME/.ssh/env"

[ -d "$HOME/bin" ]     && export PATH="$PATH:$HOME/bin"
[ -d "$HOME/scripts" ] && export PATH="$PATH:$HOME/scripts"
[ -d "$HOME/.cargo" ]  && export PATH="$PATH:$HOME/.cargo/bin"
[ -d "/usr/local/go" ] && export PATH="$PATH:/usr/local/go/bin"
[ -d "$HOME/go" ]      && export PATH="$PATH:$HOME/go/bin"
[ -d "$HOME/code/go" ] && export GOPATH="$HOME/code/go"
[ -d "$HOME/code/go" ] && export PATH="$PATH:$GOPATH/bin"

[ -f "$HOME/.bash.d/local-env-vars.sh" ] && . "$HOME/.bash.d/local-env-vars.sh"

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

#-------------------------------------------------------------------------------
# OS-Specific things
#-------------------------------------------------------------------------------
if [ "$(uname -s)" = "Linux" ]; then # Linux stuff.
    export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
    export SSH_AUTH_SOCK="$XDG_RUNTIME_DIR/ssh-agent.socket"

    alias dmenu='dmenu_run -fn "Liberation Sans:size=15" -b -sb purple'

    # Rebind keys.
    # TODO: Look up loadkeys as an alternative to xmodmap.
    [ -f ~/.Xmodmap ] && xmodmap ~/.Xmodmap

    ssh-add

    # If on tty1 and i3 isn't already running, then start it.
    if [ "$(tty)" = "/dev/tty1" ]; then
        pgrep -x i3 || exec startx
    fi

    # TODO: See pywal: https://github.com/dylanaraps/pywal
elif [ $(uname -s) = "Darwin" ]; then # OSX stuff.
    # If you need to have ncurses first in your PATH run:
    if [ -d "/usr/local/opt/ncurses" ]; then
        export PATH="$PATH:/usr/local/opt/ncurses/bin"

        # For compilers to find ncurses you may need to set:
        # TODO: Make additive instead of destructive?
        export LDFLAGS="-L/usr/local/opt/ncurses/lib"
        export CPPFLAGS="-I/usr/local/opt/ncurses/include"
    fi
fi

#-------------------------------------------------------------------------------
# Shell-specific settings.
#-------------------------------------------------------------------------------
if [ "$BASH" ]; then # Bash-specific things.
    . ~/.bashrc
fi

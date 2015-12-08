# ~/.profile: executed by the command interpreter for login shells.
# This file is not read by bash(1), if ~/.bash_profile or ~/.bash_login
# exists.
# see /usr/share/doc/bash/examples/startup-files for examples.
# the files are located in the bash-doc package.

# the default umask is set in /etc/profile; for setting the umask
# for ssh logins, install and configure the libpam-umask package.
#umask 022

# if running bash
if [ -n "$BASH_VERSION" ]; then
    # include .bashrc if it exists
    if [ -f "$HOME/.bashrc" ]; then
	      . "$HOME/.bashrc"
    fi
fi

# set PATH so it includes user's private bin if it exists
if [ -d "$HOME/bin" ] ; then
    PATH="$HOME/bin:$PATH"
fi

export LD_LIBRARY_PATH=/usr/local/lib:$LD_LIBRARY_PATH
export TERM=xterm-256color

export PATH=$PATH:${HOME}/script:${HOME}/bin
export PATH=$PATH:$HOME/code/llvm-build/Release+Asserts/bin

export EDITOR=emacs

export QT_SELECT=qt4

export GOPATH=~/code/go
export HTTPSERVICEPATH=$GOPATH/src/github.com/mogofinancial/http_service

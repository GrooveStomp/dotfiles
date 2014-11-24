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
export JAVA_HOME=${HOME}/.java/current
export CLASSPATH=/usr/local/bin/jdbc.jar:.

export PATH=$PATH:${HOME}/script:${HOME}/bin
export PATH=${HOME}/.cabal/bin:$PATH
export PATH=/opt/cabal/1.20/bin:$PATH
export PATH=/opt/ghc/7.8.2/bin:$PATH
export PATH=/opt/happy/1.19.3:$PATH
export PATH=/opt/alex/3.1.3/bin:$PATH
export PATH=${JAVA_HOME}/bin:$PATH
export PATH=$PATH:.cabal-sandbox/bin
export PATH=$PATH:$HOME/bin
export PATH=$PATH:$HOME/code/scala/current/bin
export PATH=$PATH:$HOME/code/llvm-build/Release+Asserts/bin

export EDITOR=emacs

export PATH="/usr/local/heroku/bin:$PATH"
export PATH="~/.nodejs/bin:$PATH"
export PATH="~/code/play/current:$PATH"

export QT_SELECT=qt4

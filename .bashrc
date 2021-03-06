#-------------------------------------------------------------------------------
#  _               _
# | |__   __ _ ___| |__  _ __ ___
# | '_ \ / _` / __| '_ \| '__/ __|
# | |_) | (_| \__ \ | | | | | (__
# |_.__/ \__,_|___/_| |_|_|  \___|
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
# This file can contain bash-specific configuration.
#-------------------------------------------------------------------------------

# If there is an X Server running
if [ xset q &>/dev/null ]; then
    MACHINE="$(cat /sys/devices/virtual/dmi/id/sys_vendor | tr '[:upper:]' '[:lower:]')"
    MACHINE="$MACHINE-$(cat /sys/devices/virtual/dmi/id/product_family | tr '[:upper:]' '[:lower:]')"
    echo "machine: $MACHINE"

    if [ -f "~/$MACHINE.xkb" ]; then
        xkbcomp $MACHINE.xkb
    fi
fi

eval `keychain -q --agents ssh --eval id_rsa`

# OS-specific bash configuration
. $HOME/.bashrc-$(uname | tr '[:upper:]' '[:lower:]')

# In order for gpg to find gpg-agent, gpg-agent must be running, and there must be an env
# variable pointing GPG to the gpg-agent socket. This little script, which must be sourced
# in your shell's init script (ie, .bash_profile, .zshrc, whatever), will either start
# gpg-agent or set up the GPG_AGENT_INFO variable if it's already running.

# Add the following to your shell init to set up gpg-agent automatically for every shell
if [ -n "$(pgrep gpg-agent)" ]; then
    export GPG_AGENT_INFO
else
    eval $(gpg-agent --daemon)
fi

# Terraform
if [ ! "$(which terraform 2>/dev/null)" == "" ]; then
    complete -C $(which terraform) terraform
fi

#-----------------------------------------------------------------------------
# Setup bash colors.
#
# export LSCOLORS="XxXxXxXxXxXxXxXxXxXxXx"
#                  | | | | | | | | | | └ dir w/o sticky
#                  | | | | | | | | | └ dir sticky
#                  | | | | | | | | └ exe guid
#                  | | | | | | | └ exe suid
#                  | | | | | | └ char sp
#                  | | | | | └ block sp
#                  | | | | └ exe
#                  | | | └ pipe
#                  | | └ socket
#                  | └ sym link
#                  └ dir
#
# Colors:
# a = black,   b = red,  c = green,      d = brown,  e = blue
# f = magenta, g = cyan, h = light gray, x = default
#
export LSCOLORS="GxGxxxxxCxxxxxxxxxxxxx"
export LSOPTIONS="--color=auto"
export CLICOLOR="Yes"

function compose_prompt() {
  local color_reset='\[\e[0m\]'
  local blue='\[\e[0;34m\]'

  local git_string="$(__git_ps1 %s)"
  if [ ! -z "$git_string" ]; then
    git_string="${blue}${git_string}"
  fi

  local red='\[\e[0;31m\]'
  local green='\[\e[0;32m\]'
  local purple='\[\e[0;35m\]'

  local open="$color_reset["
  local close="$color_reset]"
  local separator="$color_reset|"

  local user="${purple}\u"
  local machine="${purple}\u@\h"
  local absolute_path="${green}\w"
  local relative_path="${green}\W"

  PS1="\
${open}\
${user}${separator}\
${relative_path}${separator}\
${git_string}${close}\
\n↳ "
}

PROMPT_COMMAND=compose_prompt

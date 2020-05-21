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
function start_ssh_agent {
    ssh-agent | sed 's/^echo/#echo/' > "${SSH_ENV}"
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    ssh-add
}

# OS-specific bash configuration
. $HOME/.bashrc-$(uname | tr '[:upper:]' '[:lower:]')

if [ -f "/usr/local/share/chruby/chruby.sh" ]; then
    . /usr/local/share/chruby/chruby.sh
    [ -d "$HOME/.rubies" ] && RUBIES+=("$HOME/.rubies/*")
    [ ! -z "$WANTED_RUBY_VERSION" ] && chruby "$WANTED_RUBY_VERSION"
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

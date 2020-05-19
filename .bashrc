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

function ssh_agent_start() {
    echo "Initializing new SSH agent..."
    local readonly SSH_AGENT=$(which ssh-agent)
    $SSH_AGENT | sed 's/^echo/#echo/' > "${SSH_ENV}"
    echo succeeded
    chmod 600 "${SSH_ENV}"
    . "${SSH_ENV}" > /dev/null
    local readonly SSH_ADD=$(which ssh-add)
    $SSH_ADD
}

function ssh_agent_without_keychain() {
    # Source ssh settings, if applicable.
    if [ -f "${SSH_ENV}" ]; then
        . "${SSH_ENV}" > /dev/null
        ps -ef | grep ${SSH_AGENT_PID} | grep [s]sh-agent$ > /dev/null || {
            ssh_agent_start
        }
    else
        ssh_agent_start
    fi
}

function ssh_agent_with_keychain() {
    local readonly KEYCHAIN=$(which keychain)
    eval `$KEYCHAIN --eval --agents ssh -Q --quiet id_rsa`
}

GIT_PROMPT="$HOME/.bash.d/git-prompt.sh"
GIT_COMPLETION="$HOME/.bash.d/git-completion.bash"
if [ "$(uname -s)" = "Darwin" ]; then
    MYDIR="/Library/Developer/CommandLineTools/usr/share/git-core/git-prompt.sh"
    [ -f "$MYDIR" ] && GIT_PROMPT="$MYDIR"

    MYDIR="/Library/Developer/CommandLineTools/usr/share/git-core/git-completion.bash"
    [ -f "$MYDIR" ] && GIT_COMPLETION="$MYDIR"

    ssh_agent_without_keychain
# elif [ "$(uname -s)" = "Linux" ]; then
#     MYDIR="..."
#     [ -f "$MYDIR" ] && GIT_PROMPT="$MYDIR"
#
#     MYDIR="..."
#     [ -f "$MYDIR" ] && GIT_COMPLETION="$MYDIR"
#     if [ -z "$(which keychain)" ]; then
#         ssh_agent_without_keychain
#     else
#         ssh_agent_with_keychain
#     fi
fi
. $GIT_PROMPT
. $GIT_COMPLETION

if [ -f "/usr/local/share/chruby/chruby.sh" ]; then
    . /usr/local/share/chruby/chruby.sh
    [ -d "$HOME/.rubies" ] && RUBIES+=("$HOME/.rubies/*")
    [ ! -z "$WANTED_RUBY_VERSION" ] && chruby "$WANTED_RUBY_VERSION"
fi

#-----------------------------------------------------------------------------
# Setup bash colors.
#
# Colors:         ;  Order:
# a = black         DIR
# b = red           SYM_LINK
# c = green         SOCKET
# d = brown         PIPE
# e = blue          EXE
# f = magenta       BLOCK_SP
# g = cyan          CHAR_SP
# h = light gray    EXE_SUID
# x = default       EXE_GUID
#                   DIR_STICKY
#                   DIR_WO_STICKY
#
# export LSCOLORS="XxXxXxXxXxXxXxXxXxXxXx"
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

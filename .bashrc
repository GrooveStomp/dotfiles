. ${HOME}/.bash.d/ubuntu_defaults.sh
. ${HOME}/.bash.d/git-prompt.sh
. ${HOME}/.bash.d/aliases.sh
. ${HOME}/.bash.d/completion.sh
. ${HOME}/.bash.d/colorscheme.sh
. ${HOME}/.bash.d/completions/colorscheme.sh
. ${HOME}/.bash.d/touchpad.sh
. ${HOME}/.bash.d/completions/touchpad.sh
. ${HOME}/.bash.d/chruby.sh
. ${HOME}/.bash.d/fs.sh

# JRuby stuff.
export JAVA_OPTS='-Djruby.compile.mode=OFF -XX:+TieredCompilation -XX:TieredStopAtLevel=1'
export JRUBY_OPTS='-Xcompile.invokedynamic=false'

term_bg_file=${HOME}/.backup/.termbg

if [ $(cat ${term_bg_file}) == "light" ]; then
  colorscheme solarized-light
else
  colorscheme solarized-dark
fi

function rename-terminal() {
  echo -n -e "\033]0;$1\007"
}

#-----------------------------------------------------------------------------
# Setup bash colors.
#
# Colors:           Order:
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

function neutral_color() {
  local black='\[\e[0;30m\]'
  local reset='\[\e[0m\]'

  if [ $(cat ${term_bg_file}) == "dark" ]; then
    echo $reset
  else
    echo $black
  fi
}

function git_string() {
  local neutral=$(neutral_color)
  local blue='\[\e[0;34m\]'

  local separator="${neutral}|"

  if [ $(__git_ps1 %s) ]; then
    echo "${separator}${blue}git:$(__git_ps1 %s)${separator}"
  else
    echo "${separator}"
  fi
}

function compose_prompt() {
  local neutral=$(neutral_color)
  local red='\[\e[0;31m\]'
  local green='\[\e[0;32m\]'
  local purple='\[\e[0;35m\]'

  local open="${neutral}["
  local close="${neutral}]"
  local separator="${neutral}|"

  local machine="${purple}\u@\h"
  local path="${green}\w"
  local ruby="${red}rb:$(ruby -v | awk '{print $2}')"
  local git=$(git_string)

  PS1="${open}${machine}${separator}${path}${git}${ruby}${close}\n↳ "
}

PROMPT_COMMAND=compose_prompt

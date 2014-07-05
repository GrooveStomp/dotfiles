export KNIFE_ENV=dev
alias axe="KNIFE_ENV=prod knife"

function rbenv_version() {
  rbenv version 2> /dev/null | sed 's/\([^ ]*\).*/\1/'
}

function knife_env() {
  echo $KNIFE_ENV
}

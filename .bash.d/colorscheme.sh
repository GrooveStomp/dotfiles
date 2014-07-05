#!/bin/bash

colorscheme () {
  local termbg=${HOME}/.backup/.termbg
  local theme=$1
  local bin_path="${HOME}/code/gnome-terminal-colors-solarized"

  case $theme in
    solarized-light)
      echo light > ${termbg}
      ${bin_path}/set_light.sh
      ;;
    solarized-dark)
      echo dark > ${termbg}
      ${bin_path}/set_dark.sh
      ;;
    *)
      echo $"Unknown colortheme specified. Options are: solarized-light, solarized-dark."
      exit 1
  esac
}

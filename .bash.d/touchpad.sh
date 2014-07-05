#!/bin/bash

touchpad () {
  echo "WARNING: This script is currently hardware-specific."
  echo "         You should only use this on the Dell XPS 13."

  local arg=$1
  local touchpad_id=$(xinput list | grep Trackpad | sed 's/^.*id=//' | sed -re 's/[[:space:]]+.*//')

  case $arg in
    info)
      xinput list | grep Trackpad
      ;;
    enable)
      xinput set-prop ${touchpad_id} "Device Enabled" 1
      ;;
    disable)
      xinput set-prop ${touchpad_id} "Device Enabled" 0
      ;;
  esac
}

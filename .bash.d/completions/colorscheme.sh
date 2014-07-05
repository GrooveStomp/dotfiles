_colorscheme() {
  local cur prev opts
  COMPREPLY=()
  cur="${COMP_WORDS[COMP_CWORD]}"
  prev="${COMP_WORDS[COMP_CWORD-1]}"
  opts="solarized-light solarized-dark"

  case "${prev}" in
    file)
      COMPREPLY=( $(compgen -f ${cur}) )
      return 0
      ;;
    hostname)
      COMPREPLY=( $(compgen -A hostname ${cur}) )
      return 0
      ;;
    *)
      ;;
  esac

  COMPREPLY=( $(compgen -W "${opts}" -- ${cur}) )
}
complete -F _colorscheme colorscheme

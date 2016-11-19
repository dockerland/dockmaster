# prints dockmaster configuration as evaluatable output
config-load(){
  local globalcfg=/etc/dockmaster/dockmaster.cfg
  local usercfg=~/.dockmaster/dockmaster.cfg

  __dockmaster_paths=

  [ -e $usercfg ] || {
    mkdir -p $(dirname $usercfg) &>/dev/null
    config-defaults > $usercfg || error_perms \
      "unable to write user dockmaster configuration file"
  }

  [ -e $globalcfg ] || globalcfg=
  eval $(COP_PREFIX=__dockmaster_ $COP $globalcfg $usercfg --shell || \
    echo "error 'failed parsing configuration file(s) $globalcfg $usercfg'")
}


# prints default dockmaster configuration
config-defaults(){
  cat <<-EOF
; compostion paths
paths[] = ~/.dockmaster/compositions
paths[] = /etc/dockmaster/compositions
EOF
}


# prints the absolute path to a composition configuration file, return 1 if none
config-find-composition-configuration-file(){
  local paths=( $CWD )
  local path

  [ -z "$DOCKMASTER_COMPOSITION" ] || paths=$dockmaster_paths

  for path in ${paths[@]}; do
    [ -e "$path/dockmaster.yml" ] && {
      echo "$path/dockmaster.yml"
      return 0
    }
  done

  return 1
}

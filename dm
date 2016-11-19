#!/usr/bin/env bash
display_help() {
  cat <<-EOF

Dockmaster is yaml loving docker-compose orchestration
  https://github.com/dockerland/dockmaster

The dockmaster decorates behavior of plain docker-compose compositions with
instructions from an accompanying dockmaster.yml orchestration file. e.g.;
  * target which docker-machine(s) the compostion runs on
  * fire hooks before and after the 'docker-compose up' command
  * render the docker-compose.yml from templates (w/ support for many engines)
  * provide ad-hoc commands to docker-compose

Usage: dm [-c|--composition <composition>] <command> [arguments]

+ if a composition is not specified, dockmaster looks for dockmaster.yml in CWD.
+ if compostion is specified, dockmaster searches configured "composition_paths"
  (see the "dmc" command), and executes the first match found.

Examples:
  # start (daemonize) the compostion in current working directory.
  dm up -d

  # check the status of the registry.dockerland.org compostion
  dm -c registry.dockerland.org ps

  # execute an ad-hoc command. (add-vault is implemented within dockmaster.yml)
  dm -c registry.dockerland.org add-vault --service=api --secret=...

Options:
  -h|--help             Display Help
  -v|--version          Display Versioning Information
                        [version $SCRIPT_VERSION build $SCRIPT_BUILD]

  -c|--composition <c>  Specify the compostion

EOF

  [ -z "$1" ] && exit 0
  exit $1
}

main(){

  __cmd="dm"
  readonly SCRIPT_ENTRYPOINT="$0 $@"
  readonly SCRIPT_BUILD="@BUILD@"
  readonly SCRIPT_VERSION="@VERSION@"

  # prefer badevops-bootstrap provided commands
  [[ "$PATH" == *badevops/bin* ]] && __cmd_prefix="badevops-"

  # sanity checks
  [ $# -eq 0 ] && display_help 2
  readonly COP=$(get_cmd cop dcop) || error "dockmaster requires COP"

  config-load
  [ ${#__dockmaster_paths[@]} -eq 0 ] || error_noent \
    "! dockmaster requires at least one configured compostion path" \
    "  - refer to the dmc command for configuring paths"

  # feed autocompleter and terminate
  [[ "$@" == *"--__exec__=autocomplete"* ]] && {
    autocomplete $@
    exit $?
  }

  # determine if a compostion is specified
  case $1 in
    -h|--help) display_help ;;
    -v|--version) log "Dockmaster version $VERSION build $BUILD" ;;
    -c|--composition*)
      if [[ "$1" == *"="* ]]; then
        DOCKMASTER_COMPOSITION=${1#*=}
        shift
      else
        DOCKMASTER_COMPOSITION=$2
        shift 2
      fi
      [ -z "$DOCKMASTER_COMPOSITION" ] && error "specify a compostion"
      ;;
  esac

  # lookup working composition
  readonly DOCKMASTER_FILE=${DOCKMASTER_FILE:-$(config-find-composition-configuration-file)}
  readonly DOCKMASTER_DIR=$(dirname $DOCKMASTER_FILE 2>/dev/null)
  [ -r "$DOCKMASTER_FILE" ] || {
    [ -z "$DOCKMASTER_COMPOSITION" ] || error \
      "no dockmaster.yml found for the $DOCKMASTER_COMPOSITION compostion"
    error "missing dockmaster.yml"
  }

  log "[*] using $DOCKMASTER_FILE"


  exit $?
}

#@start dev-mode
# replaced by make (lib.d/ shell scripts get expanded inline)
readonly CWD=$( cd $(dirname $0) ; pwd -P )
for helper in $(find $CWD/lib.d/ -type f -name "*.sh"); do
  #@TODO check for errors when sourcing here
  . $helper
done
#@end dev-mode

main "$@"

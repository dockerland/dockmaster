#!/usr/bin/env bash

# source helpers if not loaded
[ $HELPERS_LOADED ] || . $BATS_TEST_DIRNAME/helpers.bash

DM="$TMPDIR/usr/local/bin/dm"
DMC="$TMPDIR/usr/local/bin/dmc"
SKIP_NETWORK_TEST=${SKIP_NETWORK_TEST:-false}
COMMANDS=( "$DM" "$DMC" )

#
# runtime fns
#

make_app(){
  (
    cd $REPO_ROOT
    make DESTDIR=$TMPDIR install
  )
  [ -x $DM ] || error "failed installing application binary"
}

[ -e $DM ] || make_app >&2

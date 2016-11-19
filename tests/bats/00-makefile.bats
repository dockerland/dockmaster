#!/usr/bin/env bats

#
# 00 - dependencies
#

load helpers

setup() {
  cd $REPO_ROOT
}

@test "makefile compiles dm and dmc" {
  rm -rf $REPO_ROOT/bin
  make
  [[ -e $REPO_ROOT/bin/dm && -e $REPO_ROOT/bin/dmc  ]]
}

@test "makefile installs dm and dmc" {
  make DESTDIR=$TMPDIR install
  [[ -e $TMPDIR/usr/local/bin/dm && -e $TMPDIR/usr/local/bin/dmc ]]
}

@test "makefile uninstalls $NAMESPACE" {
  make DESTDIR=$TMPDIR uninstall
    [[ ! -e $TMPDIR/usr/local/bin/dm && ! -e $TMPDIR/usr/local/bin/dmc ]]
}

@test "makefile cleans up" {
  make clean
  [[ ! -e $REPO_ROOT/bin/dm && ! -e $REPO_ROOT/bin/dmc ]]
}

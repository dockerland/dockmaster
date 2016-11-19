#!/usr/bin/env bats

#
# 01 - basic behavior and test prerequisites
#



load dex

setup(){
  [ -e $DEX ] || install_dex
}

@test "help exits with status code 2 when no arguments are passed" {
  for CMD in $COMMANDS; do
    run $CMD
    [ $status -eq 2 ]
  done
}

@test "help prints helpful output matching our fixture" {
  for CMD in $COMMANDS; do
    diff <(cat_fixture help-$CMD.txt) <($CMD --help)
    diff <(cat_fixture help-$CMD.txt) <($CMD -h)
  done
}
#
# @test "help is provided for all dex commands" {
#   for cmd in ${DEX_CMDS[@]} ; do
#     echo $cmd
#     run $DEX help $cmd
#     [ $status -eq 0 ]
#   done
# }
#
# @test "help is provided whenever -h or --help flags are passed to a command" {
#   for cmd in ${DEX_CMDS[@]} ; do
#     diff <($DEX $cmd -h) <($DEX help $cmd)
#     diff <($DEX $cmd --help) <($DEX help $cmd)
#   done
# }
#
# @test "help exits with status code 2 when no arguments are passed to a command" {
#   for cmd in ${DEX_CMDS[@]} ; do
#     run $DEX $cmd
#     [ $status -eq 2 ]
#   done
# }
#
# @test "help exits with status code 2 when invalid arguments are passed to a command" {
#   for cmd in ${DEX_CMDS[@]} ; do
#     { [ "$cmd" = "run" ] || [ "$cmd" = "install" ] || [ "$cmd" = "vars" ] ; } && continue
#     run $DEX $cmd invalid-argument
#     [ $status -eq 2 ]
#     [[ "$output" == *"unrecognized argument"* ]]
#   done
# }
#
# @test "help exits with status code 2 when invalid flags are passed to a command" {
#   for cmd in ${DEX_CMDS[@]} ; do
#     { [ "$cmd" = "run" ] || [ "$cmd" = "install" ] ; } && continue
#     run $DEX $cmd --invalid-flag
#     [ $status -eq 2 ]
#     [[ "$output" == *"unrecognized flag"* ]]
#   done
# }

#!/bin/bash
if [ "$1" == "build" ]; then
    find . | grep .hs | entr sh -c 'stack build --fast --pedantic; pkill a-dark-room-exe;'
    exit 0
fi
if [ "$1" == "run" ]; then
    while true; do stack exec a-dark-room-exe; done
    exit 0
fi

echo "Must specify either 'build' or 'run'"

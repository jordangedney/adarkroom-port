#!/bin/bash
if [ "$1" == "build" ]; then
    find . | grep .hs | entr sh -c 'cabal build; pkill adarkroom-port;'
    exit 0
fi
if [ "$1" == "run" ]; then
    while true; do cabal run; done
    exit 0
fi

echo "Must specify either 'build' or 'run'"

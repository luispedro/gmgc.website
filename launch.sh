#!/usr/bin/env bash

DIR="$( cd "$( dirname "${BASH_SOURCE[0]}" )" >/dev/null 2>&1 && pwd )"

trap 'kill $(jobs -p)' EXIT SIGINT

nix-shell -p elmPackages.elm --run make

cd build && python -m http.server &
"$DIR/simul-api.py" &

wait

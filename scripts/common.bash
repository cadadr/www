#!/usr/bin/env bash
# common.bash --- common functions

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

die () {
    echo $0: error: $@
    exit 2
}

say () {
    echo $0 \[$(date)\]: $@
}

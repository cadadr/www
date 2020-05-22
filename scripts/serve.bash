#!/usr/bin/env bash
# serve.bash --- serve content, respawn server when _site/ is deleted

# bash strict mode
set -euo pipefail
IFS=$'\n\t'

PID=0
DIR="$PWD/_site"                # NO FOLLOWING SLASH OR BREAKS INOTIFYWAIT
PIDFILE="$PWD/.server.pid"

die () {
    echo $0: error: $@
    exit 2
}

say () {
    echo $0 \[$(date)\]: $@
}

serve () {
    cleanup
    old="$PWD"
    cd "$DIR" || die Could not cd to "$DIR"
    python3 -m http.server 8000 --bind 127.0.0.1 2>&1 &
    echo $! > "$PIDFILE"
    cd "$old"
}

waitdel () {
    while true; do
        say Set up watcher for "$DIR"...
        inotifywait -e delete_self "$DIR"
        say "$DIR" deleted, restarting server...

        # Wait&poll till the directory is recreated.
        while [ ! -e "$DIR" ]; do
            sleep 0.1
        done

        serve
    done
}

cleanup () {
    if [[ ! -e "$PIDFILE" ]]; then
        return
    fi
    sync
    PID="$(cat $PIDFILE)" && rm "$PIDFILE"
    say Kill pid="$PID"...
    [ "0" = "$PID" ] || kill -9 "$PID" \
        || die Failed to kill preexisting server on pid "$PID"
}


trap cleanup SIGINT SIGTERM EXIT

if [ -e "$PIDFILE" ]; then
    if pgrep -a python3 | grep http\\.server >/dev/null; then
        trap - SIGINT SIGTERM EXIT
        die Stale pidfile found at "$PIDFILE", a potentially orphaned \
            server might be running.  Please kill it before proceeding.
    else
        rm "$PIDFILE"               # remove stale pidfile when no server proc found
    fi
fi


serve

waitdel

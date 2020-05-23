#!/usr/bin/env bash
# serve.bash --- serve content, respawn server when _site/ is deleted

# Copyright (c) 2020, İ. Göktuğ Kayaalp
#
# All rights reserved.
#
# Redistribution and use in source and binary forms, with or without
# modification, are permitted provided that the following conditions are met:
#
#     * Redistributions of source code must retain the above copyright
#       notice, this list of conditions and the following disclaimer.
#
#     * Redistributions in binary form must reproduce the above
#       copyright notice, this list of conditions and the following
#       disclaimer in the documentation and/or other materials provided
#       with the distribution.
#
#     * Neither the name of İ. Göktuğ Kayaalp nor the names of other
#       contributors may be used to endorse or promote products derived
#       from this software without specific prior written permission.
#
# THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
# "AS IS" AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
# LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS FOR
# A PARTICULAR PURPOSE ARE DISCLAIMED. IN NO EVENT SHALL THE COPYRIGHT
# OWNER OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT, INCIDENTAL,
# SPECIAL, EXEMPLARY, OR CONSEQUENTIAL DAMAGES (INCLUDING, BUT NOT
# LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES; LOSS OF USE,
# DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED AND ON ANY
# THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY, OR TORT
# (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT OF THE USE
# OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF SUCH DAMAGE.


. scripts/common.bash

PID=0
DIR="$PWD/_site"                # NO FOLLOWING SLASH OR BREAKS INOTIFYWAIT
PIDFILE="$PWD/.server.pid"

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
        while inotifywait -qq -e delete "$DIR/.."; do
            if [ ! -e $DIR ]; then
                say "$DIR" deleted, restarting server...

                # Wait&poll till the directory is recreated.
                while [ ! -e "$DIR" ]; do
                    sleep 0.1
                done

                serve
            fi
        done
    done
}

cleanup () {
    if [[ ! -e "$PIDFILE" ]]; then
        return
    fi
    PID="$(cat $PIDFILE)" && rm "$PIDFILE"
    say Kill pid="$PID"...
    [ "0" = "$PID" ] || kill -9 "$PID" \
        || die Failed to kill preexisting server on pid "$PID"
    if [[ $# -ge 1 && "$1" = "exit" ]]; then
        say Exiting...
        exit 3
    fi
}


trap "cleanup exit" SIGINT SIGTERM EXIT

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

#!/usr/bin/bash
# publish.bash --- Publish to Gitlab Pages

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


# POSIX strict-ish mode, beware eager pipelines!
set -euo pipefail
IFS=$'\n\t'

###

### Config vars:
HAKYLL="cabal new-run www-build --"
OUTREPO="$HOME/co/cadadr.gitlab.io"
TARGET="$OUTREPO/public"


### Functions:
die () {
    echo $0: error: $@
    exit 2
}


### Checks:
echo '>>>' Integrity checks...

test -f www.cabal || die You are in the wrong directory, cd to www repo root

test -d "$OUTREPO" || die Gitlab pages repository does not exist at \
                          the expected path: \`"$OUTREPO"\'

test -d "$TARGET" || die Gitlab pages repository should have a public/ subdir

( cd "$OUTREPO" && test -z "$(git status -s 2>/dev/null)" ) \
    || die Gitlab pages repository is dirty, commit or stash changes first

( cd "$PWD/content" && test -z "$(git status -s 2>/dev/null)" ) \
    || die Content repository is dirty, commit or stash changes first

test -z "$(git status -s 2>/dev/null)" \
    || die Repository is dirty, commit or stash changes first


### Collect info:
echo '>>>' Collect info...
commit="$(git rev-parse HEAD 2>/dev/null)"

test -n "$commit" || die Could not get HEAD commit hash

### Build:
echo '>>>' Building website...
eval "$HAKYLL build"

test -d _site || die Could not locate _site directory

exit

### Prepare Gitlab Pages repo:
echo '>>>' Preparing staging repo...
rm -r "$TARGET"
test -d "$TARGET" && die Failed to remove "$TARGET"
cp -r _site "$TARGET"
test -d "$TARGET" || die Failed to copy over files from _site to "$TARGET"

(
    cd "$TARGET" \
       && git add . \
       && git commit -m "www@$commit"
)

( cd "$OUTREPO" && test -z "$(git status -s 2>/dev/null)" ) \
    || die Gitlab pages repository dirty after new commit


### P

( cd "$OUTREPO" && git push ) || die Push from staging repo to remote failed

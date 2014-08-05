#!/bin/bash

DST=/tmp/irclist/"$1".list
mkdir -p /tmp/irclist/ || exit
"$(dirname $0)"/bin/RawList "$@" >"$DST" || exit
exec "$(dirname $0)"/view.sh "$DST" || exit

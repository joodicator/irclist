#!/bin/bash

exec cat "$@" | "$(dirname $0)"/bin/FormatList | less -R -S -x6,28,36

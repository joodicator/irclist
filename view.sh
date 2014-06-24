#!/bin/bash

exec cat "$@" | bin/FormatList | less -S -R -x6,32,40

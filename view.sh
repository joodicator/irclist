#!/bin/bash

exec cat "$@" | bin/FormatList | less -R -S -x6,32,40

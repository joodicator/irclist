#!/bin/bash

exec $(dirname $0)/bin/RawList "$@" 2>/dev/null | $(dirname $0)/view.sh

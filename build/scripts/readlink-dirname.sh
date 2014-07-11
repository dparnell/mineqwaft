#!/bin/bash

set -e

cd "$(dirname "$0")"
path="$(pwd -P)"
if [[ -n $(uname -s | grep "MINGW") ]]; then
    path="$(pwd -W)"
elif [[ -n $(uname -s | grep "CYGWIN") ]]; then
    path="$(cygpath -w "$path")"
fi
echo "$path"

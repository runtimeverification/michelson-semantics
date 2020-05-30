#!/usr/bin/env bash

set -euo pipefail

"$(dirname "$(readlink -f "$BASH_SOURCE")")/build.sh" --coverage

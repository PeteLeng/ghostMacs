#!/usr/bin/env bash
set -euo pipefail

emacs_exe="/c/dev/Emacs/emacs-31.1-UCRT64/bin/runemacs.exe"
# Resolve the profile from this script, regardless of the caller's directory.
profile_dir="$(cygpath -am -- "$(dirname -- "${BASH_SOURCE[0]}")")"

exec "$emacs_exe" --init-directory "$profile_dir" "$@"

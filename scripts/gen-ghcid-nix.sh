#!/usr/bin/env bash
# Regenerate ghcid.nix from ghcid.cabal in place.
#
# Writes via a temp file: `... > ghcid.nix` truncates ghcid.nix first, and
# since the flake's devShell reads ghcid.nix at eval time, that would brick
# the file before cabal2nix ever runs. Generate to a temp path, then move
# into place.
set -euo pipefail

repo_root=$(git rev-parse --show-toplevel)
cd "$repo_root"

if ! command -v cabal2nix >/dev/null 2>&1; then
    echo "gen-ghcid-nix: cabal2nix not on PATH; run inside 'nix develop'." >&2
    exit 1
fi

tmp=$(mktemp)
trap 'rm -f "$tmp"' EXIT
cabal2nix ./. > "$tmp"
mv "$tmp" ghcid.nix
trap - EXIT
echo "gen-ghcid-nix: wrote ghcid.nix"

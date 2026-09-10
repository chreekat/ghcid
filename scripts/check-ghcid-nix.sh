#!/usr/bin/env bash
# Regenerate ghcid.nix from ghcid.cabal and fail if the committed file is stale.
set -euo pipefail

repo_root=$(git rev-parse --show-toplevel)
cd "$repo_root"

warn () { echo >&2 "$@"; }

if ! command -v cabal2nix >/dev/null 2>&1; then
    warn "check-ghcid-nix: cabal2nix not on PATH; run inside 'nix develop'."
    exit 1
fi

generated=$(cabal2nix ./.)

if ! diff -u ghcid.nix <(printf '%s\n' "$generated"); then
    warn
    warn "check-ghcid-nix: ghcid.nix is out of sync with ghcid.cabal."
    warn "Regenerate it with:  nix develop -c scripts/gen-ghcid-nix.sh"
    warn "then 'git add ghcid.nix' and commit again."
    exit 1
fi

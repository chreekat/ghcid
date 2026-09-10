{
  description = "ghcid — GHCi based bare bones IDE";

  inputs = {
    nixpkgs.url = "https://channels.nixos.org/nixos-26.05/nixexprs.tar.zst";
    flake-compat = {
      url = "github:edolstra/flake-compat";
      flake = false;
    };
  };

  outputs = { self, nixpkgs, ... }:
    let
      systems = [ "x86_64-linux" "aarch64-linux" ];
      forAllSystems = f:
        nixpkgs.lib.genAttrs systems
          (system: f (import nixpkgs { inherit system; }));
    in
    {
      packages = forAllSystems (pkgs: rec {
        ghcid = pkgs.callPackage ./package.nix { };
        default = ghcid;
      });

      devShells = forAllSystems (pkgs: {
        # shellFor gives cabal a GHC whose global db already holds ghcid's
        # dependency closure (tests included), derived from the .cabal file.
        default = pkgs.haskellPackages.shellFor {
          packages = p: [ (pkgs.callPackage ./package.nix { }).withTests ];
          # Installs the pre-commit hook that guards ghcid.nix against drift
          # from ghcid.cabal (see scripts/check-ghcid-nix.sh). Idempotent, and
          # worktree-safe via `git rev-parse --git-path`.
          shellHook = ''
            if git rev-parse --is-inside-work-tree >/dev/null 2>&1; then
              hookdir=$(git rev-parse --git-path hooks)
              mkdir -p "$hookdir"
              ln -sf "$(git rev-parse --show-toplevel)/scripts/pre-commit" \
                "$hookdir/pre-commit"
            fi
          '';
          buildInputs = [
            pkgs.cabal-install
            pkgs.cabal2nix
          ];
        };
      });
    };
}

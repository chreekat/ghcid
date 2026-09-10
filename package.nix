# The ghcid binary with all Haskell libraries statically linked
# (justStaticExecutables); C libraries follow the calling package set.
#
# The source is .gitignore-filtered so a callPackage on a live checkout
# does not copy dist-newstyle into the store.
{ haskell, haskellPackages, nix-gitignore }:

let
  # Checks stay enabled here so the package's test dependencies are part of
  # its build-input closure; the flake devShell derives its GHC from this via
  # shellFor and needs them to run `cabal test`.
  #
  # ghcid.nix is the committed cabal2nix output (kept in sync by the
  # pre-commit hook in scripts/); callPackage on it avoids the IFD a live
  # callCabal2nix would incur.
  withTests =
    (haskellPackages.callPackage ./ghcid.nix { }).overrideAttrs (_: {
      src = nix-gitignore.gitignoreSource [ ] ./.;
    });
  # dontCheck: the test suite drives a real ghci over a live source tree —
  # it runs in the dev shell (`cabal test`), not in the build sandbox.
  base = haskell.lib.dontCheck withTests;
in
haskell.lib.overrideCabal
  (haskell.lib.justStaticExecutables base)
  (drv: {
    # shellFor derives the devShell's GHC from this; checks-enabled so test
    # deps come along.
    passthru = (drv.passthru or { }) // { inherit withTests; };
  })

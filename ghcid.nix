{ mkDerivation, ansi-terminal, async, base, base64-bytestring
, bytestring, cmdargs, containers, cryptohash-sha256, directory
, extra, filepath, fsnotify, lib, microaeson, network, process
, tasty, tasty-hunit, temporary, terminal-size, text, time, unix
}:
mkDerivation {
  pname = "ghcid";
  version = "1.0.0";
  src = ./.;
  isLibrary = true;
  isExecutable = true;
  libraryHaskellDepends = [
    ansi-terminal base cmdargs directory extra filepath process time
    unix
  ];
  executableHaskellDepends = [
    ansi-terminal async base base64-bytestring bytestring cmdargs
    containers cryptohash-sha256 directory extra filepath fsnotify
    microaeson network process temporary terminal-size text time unix
  ];
  testHaskellDepends = [
    ansi-terminal async base base64-bytestring bytestring cmdargs
    containers cryptohash-sha256 directory extra filepath fsnotify
    microaeson network process tasty tasty-hunit temporary
    terminal-size text time unix
  ];
  homepage = "https://github.com/ndmitchell/ghcid#readme";
  description = "GHCi based bare bones IDE";
  license = lib.licenses.bsd3;
  mainProgram = "ghcid";
}

with import (fetchTarball https://github.com/NixOS/nixpkgs/archive/0d4431cfe90b2242723ccb1ccc90714f2f68a609.tar.gz) { };

let
  hsPkgs = haskell.packages.ghc802;
in
  haskell.lib.buildStackProject {
     name = "cardano-sl";
     ghc = hsPkgs.ghc;
     buildInputs = [
       zlib openssh autoreconfHook openssl
       gmp rocksdb git bsdiff libcxx
     # cabal-install and stack pull in lots of dependencies on OSX so skip them
     # See https://github.com/NixOS/nixpkgs/issues/21200
     ] ++ (lib.optionals stdenv.isLinux [ cabal-install stack ]);
     # Needed to be changed to clever conditional
     CSL_SYSTEM_TAG = "linux64";
  }

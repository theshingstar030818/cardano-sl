with (import (fetchTarball https://github.com/NixOS/nixpkgs/archive/0d4431cfe90b2242723ccb1ccc90714f2f68a609.tar.gz) {});

stdenv.mkDerivation {
  name = "daedalus-bridge";

  buildInputs = [ nodejs-7_x nodePackages.bower purescript ];

  src = null;
}

let
  pkgs' = import <nixpkgs> { config = {}; };
in
{ pkgs ? pkgs' }:
with pkgs.lib;
with (import <nixpkgs/pkgs/development/haskell-modules/lib.nix> { inherit pkgs;});
let
  addConfigureFlags = flags: drv: overrideCabal drv (drv: {
    configureFlags = flags;
  });
  ghc = pkgs.haskell.compiler.ghc802.overrideAttrs (old: {
    src = pkgs.fetchurl {
      url = "https://github.com/nh2/ghc/releases/download/ghc-8.0.2-base-extra-libraries-m-fa3f3487/ghc-8.0.2-src.tar.xz";
      sha256 = "03l8nfm3cm2qym36wjscp1lfmjb2iiikf8p3vn436alznh8dscp8";
    };
  });
  # compiler = pkgs.haskell.packages.ghc802;
  ghc802 = pkgs'.callPackage <nixpkgs/pkgs/development/haskell-modules> {
    inherit ghc;
    compilerConfig = pkgs'.callPackage <nixpkgs/pkgs/development/haskell-modules/configuration-ghc-8.0.x.nix> { };
  };
  compiler = ghc802;
in ((import ./pkgs { inherit pkgs ghc compiler; }).override {
  overrides = self: super: {
    cardano-sl = overrideCabal super.cardano-sl (drv: {
      src = cleanSource drv.src;
      doHaddock = false;
      patchPhase = ''
       export CSL_SYSTEM_TAG=linux64
      '';
      # production full nodes shouldn't use wallet as it means different constants
      configureFlags = [
        "-f-asserts"
        "-f-dev-mode"
        "-f-with-explorer"
        # https://github.com/NixOS/nixpkgs/pull/24692#issuecomment-306509337
        # "--ghc-option=-optl-lm"
      ];
      doCoverage = true;
    });
    cardano-sl-core = overrideCabal super.cardano-sl-core (drv: {
      configureFlags = [
        "-fembed-config"
      ];
    });
    cardano-sl-static = justStaticExecutables self.cardano-sl;
    cardano-without-explorer = disableCabalFlag self.cardano-sl "with-explorer";
    cardano-with-explorer = disableCabalFlag self.cardano-sl "with-explorer";    # Gold linker fixes
    cryptonite = addConfigureFlags ["--ghc-option=-optl-pthread"] super.cryptonite;
  };
}) // {
  stack2nix = pkgs.haskellPackages.callPackage ./pkgs/stack2nix.nix {};
}

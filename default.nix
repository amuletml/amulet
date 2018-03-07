let
  rev = "7a04c2ca296c0698f1c7d5c17be7f931f77691f7";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "18d8gxzbc3vwlr68ppxg575wlf28bxgck0mbhms1bf1l225gw5dk";
  };
  nixpkgs = import pkgs { config = {}; };
in { compiler ? "ghc822" }:

let
  inherit (nixpkgs) pkgs haskell;

  f = { mkDerivation, stdenv
      , mtl
      , syb
      , text
      , base
      , lens
      , array
      , hedgehog
      , monad-gen
      , bytestring
      , containers
      , transformers
      , pretty-show
      , annotated-wl-pprint
      , alex
      , happy
      }:
      let alex' = haskell.lib.dontCheck alex;
          happy' = haskell.lib.dontCheck happy;
      in mkDerivation {
        pname = "amuletml";
        version = "0.1.0.0";
        src = ./.;

        isLibrary = false;
        isExecutable = true;

        libraryHaskellDepends = [
          annotated-wl-pprint array base bytestring containers lens monad-gen
          mtl pretty-show syb text transformers
        ];

        executableHaskellDepends = [
          mtl text base lens monad-gen bytestring containers pretty-show
        ];

        testHaskellDepends = [
          base bytestring hedgehog lens monad-gen mtl pretty-show text
        ];

        libraryToolDepends = [ alex' happy' ];
        buildDepends = [ alex' happy' pkgs.cabal-install ];

        homepage = "https://amulet.ml";
        description = "A functional programming language";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in
  if pkgs.lib.inNixShell then drv.env else drv

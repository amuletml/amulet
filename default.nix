{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

let

  inherit (nixpkgs) pkgs haskell;

  f = { mkDerivation, stdenv
      , mtl
      , syb
      , text
      , base
      , lens
      , array
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

        libraryToolDepends = [ alex' happy' ];
        buildDepends = [ alex' happy' ];

        homepage = "https://amulet.ml";
        description = "A functional programming language";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in
  if pkgs.lib.inNixShell then drv.env else drv

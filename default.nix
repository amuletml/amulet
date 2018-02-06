{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, stdenv
      , mtl
      , syb
      , text
      , array
      , bytestring
      , base
      , lens
      , parsec
      , monad-gen
      , containers
      , transformers
      , pretty-show
      , alex
      , happy
      }:
      mkDerivation {
        pname = "amuletml";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          mtl syb text array bytestring base
          lens parsec monad-gen containers transformers
          pretty-show
        ];
        buildDepends = [ alex happy ];
        homepage = "https://amulet.ml";
        description = "A functional programming language";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in
  if pkgs.lib.inNixShell then drv.env else drv

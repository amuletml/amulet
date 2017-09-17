{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc821" }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, base, comonad, containers, monad-gen, mtl
      , parsec, stdenv, text, transformers
      }:
      mkDerivation {
        pname = "amuletml";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base comonad containers monad-gen mtl parsec text transformers
        ];
        homepage = "https://amulet.ml";
        description = "A functional programming language";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = pkgs.haskell.packages.${compiler};
  drv = haskellPackages.callPackage f {};
in if pkgs.lib.inNixShell then drv.env else drv

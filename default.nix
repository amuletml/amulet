{ nixpkgs ? import <nixpkgs> {}, compiler ? "ghc822", doBenchmark ? false }:

let

  inherit (nixpkgs) pkgs;

  f = { mkDerivation, alex, base, containers, happy, monad-gen, mtl
      , parsec, pretty-show, stdenv, syb, text, transformers
      }:
      mkDerivation {
        pname = "amuletml";
        version = "0.1.0.0";
        src = ./.;
        isLibrary = false;
        isExecutable = true;
        executableHaskellDepends = [
          base containers monad-gen mtl parsec pretty-show syb text
          transformers alex happy
        ];
        homepage = "https://amulet.ml";
        description = "A functional programming language";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

  variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

  drv = variant (haskellPackages.callPackage f {});

in

  if pkgs.lib.inNixShell then drv.env else drv

let
  rev = "cd960b965f2587efbe41061a4dfa10fc72a28781";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "0k2pk3y54kh2lz8yaj2438sqclayhsc0b2w262qb6iwyafby8pr0";
  };
  nixpkgs = import pkgs {
    config = {
      packageOverrides = pkgs_: with pkgs_; {
        haskell = haskell // {
          packages = haskell.packages // {
            ghc842-profiling = haskell.packages.ghc842.override {
              overrides = self: super: {
                mkDerivation = args: super.mkDerivation (args // {
                  enableLibraryProfiling = true;
                });
              };
            };
          };
        };
      };
    };
  };
in { compiler ? "ghc842", ci ? false }:

let
  inherit (nixpkgs) pkgs haskell;

  f = { mkDerivation, stdenv
      , mtl
      , syb
      , text
      , base
      , lens
      , array
      , Diff
      , HUnit
      , tasty
      , hedgehog
      , directory
      , monad-gen
      , bytestring
      , containers
      , transformers
      , pretty-show
      , annotated-wl-pprint
      , tasty-hunit
      , tasty-hedgehog_0_2_0_0
      , alex
      , happy
      }:
      let alex' = haskell.lib.dontCheck alex;
          happy' = haskell.lib.dontCheck happy;
      in mkDerivation rec {
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
          base bytestring Diff directory hedgehog HUnit lens monad-gen mtl
          pretty-show tasty tasty-hedgehog_0_2_0_0 tasty-hunit text
        ];

        libraryToolDepends = if ci then [ alex happy ] else [ alex' happy' ];
        buildDepends = libraryToolDepends ++ [ pkgs.cabal-install ];

        homepage = "https://amulet.ml";
        description = "A functional programming language";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in
  if pkgs.lib.inNixShell then drv.env else drv

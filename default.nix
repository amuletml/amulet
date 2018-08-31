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
            ghc842 = haskell.packages.ghc842.override {
              overrides = self: super: {
                io-capture = haskell.lib.overrideCabal super.io-capture (old: rec {
                  doCheck = false;
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
      , alex
      , base
      , Diff
      , lens
      , text
      , array
      , happy
      , hlint
      , hslua
      , HUnit
      , tasty
      , hashable
      , hedgehog
      , directory
      , haskeline
      , bytestring
      , containers
      , pretty-show
      , tasty-hunit
      , transformers
      , tasty-ant-xml
      , template-haskell
      , annotated-wl-pprint
      , unordered-containers
      , tasty-hedgehog_0_2_0_0
      }:
      let alex' = haskell.lib.dontCheck alex;
          happy' = haskell.lib.dontCheck happy;
          hlint' = haskell.lib.dontCheck hlint;
      in mkDerivation rec {
        pname = "amuletml";
        version = "0.1.0.0";
        src = ./.;

        isLibrary = false;
        isExecutable = true;

        libraryHaskellDepends = [
          annotated-wl-pprint array base bytestring containers lens
          mtl pretty-show syb text transformers template-haskell hashable
          unordered-containers
        ];

        executableHaskellDepends = [
          mtl text base lens bytestring containers pretty-show hslua
          haskeline
        ];

        testHaskellDepends = [
          base bytestring Diff directory hedgehog HUnit lens mtl
          pretty-show tasty tasty-hedgehog_0_2_0_0 tasty-hunit text
          tasty-ant-xml
        ];

        libraryToolDepends = if ci then [ alex happy ] else [ alex' happy' ];
        buildDepends = libraryToolDepends ++ [ pkgs.cabal-install ] ++ [ hlint' ];

        homepage = "https://amulet.ml";
        description = "A functional programming language";
        license = stdenv.lib.licenses.bsd3;
      };

  haskellPackages = pkgs.haskell.packages.${compiler};

  drv = haskellPackages.callPackage f {};

in
  if pkgs.lib.inNixShell then drv.env else drv

let
  rev = "0b97a9c4755ee71e64bac9408c766f13a930999a";
  pkgs = builtins.fetchTarball {
    url = "https://github.com/NixOS/nixpkgs/archive/${rev}.tar.gz";
    sha256 = "030knsmkqy7fhpi8dsxhm76lvhak551bvzicgsnjidhwv063bw32";
  };
  nixpkgs = import pkgs {
    config = {
      packageOverrides = pkgs_: with pkgs_; {
        haskell = haskell // {
          packages = haskell.packages // {
            ghc861-profiling = haskell.packages.ghc861.override {
              overrides = self: super: {
                mkDerivation = args: super.mkDerivation (args // {
                  enableLibraryProfiling = true;
                });
              };
            };
            ghc861 = haskell.packages.ghc861.override {
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
in { compiler ? "ghc861", ci ? false }:

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
      , these
      , filepath
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
      , optparse-applicative
      , tasty-hedgehog
      }:
      let alex' = haskell.lib.dontCheck alex;
          happy' = haskell.lib.dontCheck happy;
          hlint' = haskell.lib.dontCheck hlint;
      in mkDerivation rec {
        pname = "amuletml";
        version = "0.1.0.0";
        src = if pkgs.lib.inNixShell then null else ./.;

        isLibrary = false;
        isExecutable = true;

        libraryHaskellDepends = [
          annotated-wl-pprint array base bytestring containers lens
          mtl pretty-show syb text transformers template-haskell hashable
          unordered-containers these
        ];

        executableHaskellDepends = [
          mtl text base lens bytestring containers pretty-show hslua
          haskeline optparse-applicative
        ];

        testHaskellDepends = [
          base bytestring Diff directory filepath hedgehog HUnit lens mtl
          pretty-show tasty tasty-hedgehog tasty-hunit text
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

{ compiler ? "ghc901" }:

let
  config = {
    # need to enable these for build inputs in nix-shell
    allowUnsupportedSystem = true;
    allowBroken = true;
    allowUnfree = true;

    # other configuration for some packages, we dont need these to actually work
    cplex.releasePath = "/dev/null";

    packageOverrides = pkgs: rec {
      haskell = pkgs.haskell // {
        packages = pkgs.haskell.packages // {
          "${compiler}" = pkgs.haskell.packages.${compiler}.override {
            overrides = haskellPackagesNew: haskellPackagesOld: rec {

              ghc-build-diagnostics = haskellPackagesNew.callPackage ./default.nix {};
              mkDerivation = args: haskellPackagesOld.mkDerivation (args // {
                doCheck = false;
              });
              hs-colour = haskellPackagesNew.callPackage ./nix/hscolour.nix {};
              # unordered-containers = haskellPackagesNew.callPackage ./unordered-containers.nix {};
            };
          };
        };
      };
    };
  };

  pkgs = import <nixpkgs> { inherit config; };
  # unstable = import <unstable> { };

in
  rec {
    ghc-build-diagnostics = pkgs.haskell.packages.${compiler}.ghc-build-diagnostics;

    shell = pkgs.haskell.packages.${compiler}.shellFor {
      packages = p: [ghc-build-diagnostics];
      withIDe = true;
      buildInputs = with pkgs; [ zlib
                                 numactl # required for some packages such as pandoc
                                 gmp     # required for sbv
                                 cabal-install
                                 wget
                               ]
      ++
      (with haskellPackages; [ hlint
                               stylish-haskell
                               hasktags
                               apply-refact
                               hindent
                               ghcide
                               ghc-prof-flamegraph
                               profiteur
                               conduit
                               containers_0_6_4_1
                             ]);
    };
  }

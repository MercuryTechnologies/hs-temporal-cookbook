{
  description = "";

  nixConfig = {
    # This sets the flake to use several upstream substituters that have cached
    # artifacts for some of our dependencies.
    #
    # Nix should ask for permission before using it, but remove it here if you
    # do not want it to.
    extra-substituters = [ "https://cache.garnix.io" ];
    extra-trusted-public-keys = [
      "cache.garnix.io:CTFPyKSLcx5RMJKfLo5EEPUObbA78b0YQ2DTCJXqr9g="
    ];
  };

  outputs =
    inputs:
    inputs.flake-parts.lib.mkFlake { inherit inputs; } {
      systems = inputs.nixpkgs.lib.systems.flakeExposed;
      perSystem =
        { pkgs, system, ... }:
        {
          _module.args.pkgs = import inputs.nixpkgs {
            inherit system;
            overlays = [
              inputs.hs-temporal-sdk.overlays.temporal-bridge
              inputs.hs-temporal-sdk.overlays.temporal-test-server
              inputs.hs-temporal-sdk.overlays.haskell-development
              # FIXME: Drop this once we update nixpkgs to a more recent
              # 'unstable', which includes an updated 'otel-desktop-viewer'.
              (final: prev: prev.lib.packagesFromDirectoryRecursive {
                inherit (final) callPackage;
                directory = ./nix;
              })
            ];
          };

          devShells =
            let
              mkShell =
                { ghcVersion ? "ghc910", extraPkgs ? [], extraEnv ? {}, ... }:
                pkgs.haskell.packages.${ghcVersion}.shellFor {
                  name = "hs-temporal-cookbook";
                  packages = hpkgs: [
                    (hpkgs.callCabal2nix "hello" ./hello { })
                  ];
                  withHoogle = true;
                  nativeBuildInputs = (with pkgs; [
                    cabal-install
                    ghcid
                    openjdk
                    ormolu
                    temporal-cli
                    temporal-test-server
                   ];
                  ]) ++ extraPkgs;
                  env = extraEnv;
                };
            in
            rec {
              default = ghc910;
              ghc96 = mkShell { ghcVersion = "ghc96"; };
              ghc98 = mkShell { ghcVersion = "ghc98"; };
              ghc910 = mkShell { };
              otel = mkShell {
                extraPkgs = [pkgs.otel-desktop-viewer];
              };
            };
        };
    };

  inputs = {
    # reference the package set used to build/test `hs-temporal-sdk` to avoid
    # rebuilding artifacts that have already been cached by upstream CI.
    nixpkgs.follows = "hs-temporal-sdk/nixpkgs";

    flake-parts = {
      url = "github:hercules-ci/flake-parts";
      inputs.nixpkgs-lib.follows = "nixpkgs";
    };

    hs-temporal-sdk = {
      url = "github:MercuryTechnologies/hs-temporal-sdk";
      inputs = {
        # stubbed out to avoid bloating `flake.lock`
        devenv.follows = "";
        fenix.follows = "";
      };
    };
  };
}

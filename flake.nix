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
              inputs.hs-temporal-sdk.overlays.haskell-development
            ];
          };

          devShells =
            let
              mkShell =
                ghcVersion:
                pkgs.haskell.packages.${ghcVersion}.shellFor {
                  name = "hs-temporal-cookbook";
                  packages = hpkgs: [
                    (hpkgs.callCabal2nix "hello" ./hello { })
                  ];
                  withHoogle = true;
                  nativeBuildInputs = with pkgs; [
                    act
                    cabal-install
                    ghcid
                    openjdk
                    ormolu
                    temporal-cli
                  ];
                };
            in
            rec {
              ghc96 = mkShell "ghc96";
              ghc98 = mkShell "ghc98";
              ghc910 = mkShell "ghc910";
              default = ghc910;
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
      url = "github:MercuryTechnologies/hs-temporal-sdk/jkachmar/mock-activity-environment";
      inputs = {
        # stubbed out to avoid bloating `flake.lock`
        devenv.follows = "";
        fenix.follows = "";
      };
    };
  };
}

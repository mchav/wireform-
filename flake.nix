{
  description = "wireform — high-performance multi-format serialization toolkit for Haskell";

  inputs = {
    nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";
    flake-utils.url = "github:numtide/flake-utils";
  };

  outputs = { self, nixpkgs, flake-utils }:
    flake-utils.lib.eachDefaultSystem (system:
      let
        pkgs = import nixpkgs { inherit system; };
        lib = pkgs.lib;
        hlib = pkgs.haskell.lib.compose;

        # GHC build matrix — add/remove entries to test against different compilers.
        # Shell names become `nix develop .#ghcXY`.
        ghcMatrix = {
          ghc96  = "ghc96";
          ghc98  = "ghc98";
          ghc910 = "ghc910";
        };

        defaultGHC = "ghc98";

        systemDeps = [
          pkgs.zlib
          pkgs.snappy
          pkgs.zstd
          pkgs.lz4
        ];

        sharedTools = [
          pkgs.cabal-install
          pkgs.pkg-config
          pkgs.ghciwatch
          pkgs.haskellPackages.fourmolu
          pkgs.hlint
          pkgs.prek
        ];

        haskellOverlay = self: super: {
          wireform =
            hlib.enableCabalFlag "lz4"
              (hlib.enableCabalFlag "zstd"
                (hlib.enableCabalFlag "snappy"
                  (hlib.overrideCabal (drv: {
                # Benchmarks pull in proto-lens / criterion / xeno / hexml
                # which aren't needed for dev and may not be in the set.
                doBenchmark = false;
              }) (self.callCabal2nix "wireform" ./. {}))));
        };

        mkDevShell = ghcAttr:
          let
            hp = (pkgs.haskell.packages.${ghcAttr}).override {
              overrides = haskellOverlay;
            };
          in
          hp.shellFor {
            # wireform core gets nix-prebuilt deps; wireform-grpc deps
            # are built by cabal (its grpc-spec dep chain has nixpkgs
            # platform issues on aarch64-darwin).
            packages = p: [ p.wireform ];

            nativeBuildInputs = [
              hp.haskell-language-server
            ] ++ sharedTools;

            buildInputs = systemDeps;

            shellHook = ''
              echo "wireform dev shell — $(ghc --version)"
            '';
          };

        devShells = builtins.mapAttrs (_: mkDevShell) ghcMatrix;
      in
      {
        devShells = devShells // {
          default = devShells.${defaultGHC};
        };
      }
    );
}

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

        # GHC build matrix — add/remove entries to test against
        # different compilers. Shell names become `nix develop .#ghcXY`.
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
          pkgs.llvmPackages.llvm
        ];

        # ------------------------------------------------------------
        # Per-format package set
        # ------------------------------------------------------------
        # Every per-format package gets its own callCabal2nix entry;
        # the umbrella `wireform` then depends on the lot. Without
        # this list, cabal2nix would try to resolve names like
        # `wireform-proto` against Hackage and fail.
        #
        # Adding a new per-format package means: drop a line into
        # `wireformPackages` *and* an entry under `cabal.project`
        # at the workspace root.
        wireformPackages = {
          wireform-core         = ./wireform-core;
          wireform-derive       = ./wireform-derive;
          wireform-columnar     = ./wireform-columnar;
          wireform-proto        = ./wireform-proto;
          wireform-avro         = ./wireform-avro;
          wireform-thrift       = ./wireform-thrift;
          wireform-cbor         = ./wireform-cbor;
          wireform-msgpack      = ./wireform-msgpack;
          wireform-bson         = ./wireform-bson;
          wireform-ion          = ./wireform-ion;
          wireform-capnproto    = ./wireform-capnproto;
          wireform-flatbuffers  = ./wireform-flatbuffers;
          wireform-bond         = ./wireform-bond;
          wireform-asn1         = ./wireform-asn1;
          wireform-xml          = ./wireform-xml;
          wireform-html         = ./wireform-html;
          wireform-parquet      = ./wireform-parquet;
          wireform-orc          = ./wireform-orc;
          wireform-arrow        = ./wireform-arrow;
          wireform-iceberg      = ./wireform-iceberg;
          wireform-edn          = ./wireform-edn;
          wireform-bencode      = ./wireform-bencode;
          wireform-toml         = ./wireform-toml;
          wireform-csv          = ./wireform-csv;
          wireform-ndjson       = ./wireform-ndjson;
          wireform-grpc         = ./wireform-grpc;
        };

        # Cabal flags to enable on specific packages. Mirrors the
        # `package <name>` blocks in `cabal.project`.
        packageFlags = {
          wireform = [ "snappy" "zstd" ];
          wireform-arrow = [ "zstd" "lz4" ];
        };

        applyFlags = name: drv:
          lib.foldl' (acc: flag: hlib.enableCabalFlag flag acc) drv
            (packageFlags.${name} or []);

        # Build every per-format package via callCabal2nix and
        # apply the right Cabal flags. Benchmarks are off by
        # default to avoid pulling proto-lens / criterion / xeno
        # / hexml into the closure.
        haskellOverlay = self: super:
          let
            mkPkg = name: src:
              applyFlags name
                (hlib.overrideCabal (drv: { doBenchmark = false; })
                  (self.callCabal2nix name src {}));
            perFormatAttrs = lib.mapAttrs mkPkg wireformPackages;
            wireformAttr = applyFlags "wireform"
              (hlib.overrideCabal (drv: { doBenchmark = false; })
                (self.callCabal2nix "wireform" ./. {}));
          in
            perFormatAttrs // { wireform = wireformAttr; };

        mkDevShell = ghcAttr:
          let
            hp = (pkgs.haskell.packages.${ghcAttr}).override {
              overrides = haskellOverlay;
            };
            # Every package the workspace ships, so a single
            # `nix develop` shell can build any of them via
            # `cabal build <pkg>`. The closure is rebuilt only when
            # the dep set actually changes.
            workspaceDrvs =
              [ hp.wireform ]
              ++ lib.attrValues
                   (lib.getAttrs (lib.attrNames wireformPackages) hp);
          in
          hp.shellFor {
            packages = _: workspaceDrvs;

            nativeBuildInputs = [
              hp.haskell-language-server
            ] ++ sharedTools;

            buildInputs = systemDeps;

            shellHook = ''
              echo "wireform dev shell — $(ghc --version)"
              echo "  packages: ${
                toString (lib.attrNames wireformPackages)} wireform"
            '';
          };

        devShells = builtins.mapAttrs (_: mkDevShell) ghcMatrix;
      in
      {
        devShells = devShells // {
          default = devShells.${defaultGHC};
        };

        # Every per-format package is also exposed as a build
        # output for downstream Nix consumers. The umbrella
        # `wireform` is the default.
        packages =
          let hp = (pkgs.haskell.packages.${defaultGHC}).override {
                overrides = haskellOverlay;
              };
              perFormat = lib.getAttrs (lib.attrNames wireformPackages) hp;
          in perFormat // {
            wireform = hp.wireform;
            default  = hp.wireform;
          };
      }
    );
}

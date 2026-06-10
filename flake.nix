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
        # Covers the released GHC 9.4–9.14 line (odd minors are
        # unreleased dev snapshots and are intentionally omitted).
        ghcMatrix = {
          ghc94  = "ghc94";
          ghc96  = "ghc96";
          ghc98  = "ghc98";
          ghc910 = "ghc910";
          ghc912 = "ghc912";
          ghc914 = "ghc914";
        };

        defaultGHC = "ghc98";

        systemDeps = [
          pkgs.zlib
          pkgs.snappy
          pkgs.zstd
          pkgs.lz4
          pkgs.brotli
          pkgs.openssl
        ];

        sharedTools = [
          pkgs.cabal-install
          pkgs.pkg-config
          pkgs.ghciwatch
          pkgs.haskellPackages.fourmolu
          pkgs.hlint
          pkgs.prek
          pkgs.llvmPackages.llvm
          # protoc is required at configure time by
          # proto-lens-protobuf-types, which the wireform-grpc test
          # suites pull in. It also unblocks anyone running the
          # protobuf conformance / interop scripts manually.
          pkgs.protobuf
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
          wireform-core              = ./wireform-core;
          wireform-derive            = ./wireform-derive;
          wireform-columnar-core     = ./wireform-columnar-core;
          wireform-columnar          = ./wireform-columnar;
          wireform-proto             = ./wireform-proto;
          wireform-avro              = ./wireform-avro;
          wireform-thrift            = ./wireform-thrift;
          wireform-cbor              = ./wireform-cbor;
          wireform-msgpack           = ./wireform-msgpack;
          wireform-bson              = ./wireform-bson;
          wireform-ion               = ./wireform-ion;
          wireform-capnproto         = ./wireform-capnproto;
          wireform-flatbuffers       = ./wireform-flatbuffers;
          wireform-bond              = ./wireform-bond;
          wireform-asn1              = ./wireform-asn1;
          wireform-xml               = ./wireform-xml;
          wireform-html              = ./wireform-html;
          wireform-parquet           = ./wireform-parquet;
          wireform-orc               = ./wireform-orc;
          wireform-arrow             = ./wireform-arrow;
          wireform-iceberg           = ./wireform-iceberg;
          wireform-edn               = ./wireform-edn;
          wireform-bencode           = ./wireform-bencode;
          wireform-toml              = ./wireform-toml;
          wireform-csv               = ./wireform-csv;
          wireform-ndjson            = ./wireform-ndjson;
          wireform-fory              = ./wireform-fory;
          wireform-network           = ./wireform-network;
          wireform-attoparsec        = ./wireform-attoparsec;
          wireform-binary            = ./wireform-binary;
          wireform-cereal            = ./wireform-cereal;
          wireform-http1             = ./wireform-http1;
          wireform-http2             = ./wireform-http2;
          wireform-http              = ./wireform-http;
          wireform-http-wai          = ./wireform-http-wai;
          wireform-kafka-protocol    = ./wireform-kafka-protocol;
          wireform-kafka             = ./wireform-kafka;
          wireform-stats             = ./wireform-stats;
          wireform-lance             = ./wireform-lance;
          wireform-yaml              = ./wireform-yaml;
          wireform-delta             = ./wireform-delta;
          wireform-hudi              = ./wireform-hudi;
          wireform-grpc              = ./wireform-grpc;
          # Previously only present transitively / via a one-off
          # callCabal2nix; listed here so each is exposed as a
          # per-GHC package output the CI matrix can build.
          hermes                     = ./hermes;
          grpc-spec                  = ./grpc-spec;
          wireform-websocket         = ./wireform-websocket;
          wireform-cel               = ./wireform-cel;
          wireform-protovalidate     = ./wireform-protovalidate;
          wireform-hw-kafka-client   = ./wireform-hw-kafka-client;
        };

        # Cabal flags to enable on specific packages.
        #
        # NOTE: the optional codec flags (+snappy/+zstd/+lz4) are
        # deliberately NOT enabled for the `nix build` outputs. Under
        # cabal2nix, `extra-libraries: zstd` and the Haskell
        # `build-depends: zstd` both resolve to `haskellPackages.zstd`,
        # and the overlay binds that name to the C library (pkgs.zstd)
        # so `extra-libraries` link. Enabling the flag would then ask
        # for the Haskell `zstd` package under the same (now-shadowed)
        # name and fail with "missing or private dependencies: zstd".
        # The codec paths are exercised by the `cabal test` step, which
        # solves against Hackage rather than the pinned nix package set.
        packageFlags = {
        };

        applyFlags = name: drv:
          lib.foldl' (acc: flag: hlib.enableCabalFlag flag acc) drv
            (packageFlags.${name} or []);

        # Build every per-format package via callCabal2nix and
        # apply the right Cabal flags. Benchmarks and tests are
        # both off at the Nix level: tests belong in `cabal test`,
        # not in the Nix sandbox (ring-buffer / OS-specific tests
        # misbehave under sandboxing, and dependency-cycle
        # packages like wireform-columnar-core would deadlock the
        # build graph). Use `cabal test` inside the dev shell instead.
        # wireform-kafka-protocol's library reads its sources from a
        # sibling tree (`hs-source-dirs: ../wireform-kafka/src`). A bare
        # callCabal2nix copies only the package directory, so the sibling
        # path is absent in the sandbox ("can't find source for
        # Kafka/Protocol/Generated/..."). Stitch both directories into a
        # single source root and point cabal2nix at the subpath.
        kafkaProtocolSrc = pkgs.runCommand "wireform-kafka-protocol-src" {} ''
          mkdir -p $out/wireform-kafka-protocol $out/wireform-kafka
          cp -R ${./wireform-kafka-protocol}/. $out/wireform-kafka-protocol/
          cp -R ${./wireform-kafka/src} $out/wireform-kafka/src
        '';

        haskellOverlay = self: super:
          let
            mkRaw = name: src:
              if name == "wireform-kafka-protocol"
              then self.callCabal2nixWithOptions name kafkaProtocolSrc
                     "--subpath wireform-kafka-protocol" {}
              else self.callCabal2nix name src {};
            mkPkg = name: src:
              applyFlags name
                (hlib.overrideCabal (drv: {
                  doBenchmark = false;
                  doCheck    = false;
                })
                  (mkRaw name src));
            perFormatAttrs = lib.mapAttrs mkPkg wireformPackages;
            wireformAttr = applyFlags "wireform"
              (hlib.overrideCabal (drv: { doBenchmark = false; })
                (self.callCabal2nix "wireform" ./. {}));
            # crc32c-0.2.2 in nixpkgs lists only x86 Darwin as supported,
            # but Google's crc32c C library has ARMv8 CRC hardware support.
            # Lift the false platform restriction so grpc-spec (and
            # wireform-grpc) resolve on aarch64-darwin.
            crc32cUnrestricted = super.crc32c.overrideAttrs (old: {
              meta = old.meta // { platforms = lib.platforms.all; };
            });
          in
            perFormatAttrs // {
              wireform = wireformAttr;
              crc32c   = crc32cUnrestricted;
              # Map pkg-config names (cabal `pkgconfig-depends`) and
              # bare C library names (cabal `extra-libraries`) to
              # system packages so cabal2nix-generated derivations
              # can find them.
              liblz4   = pkgs.lz4;
              lz4      = pkgs.lz4;
              snappy   = pkgs.snappy;
              libzstd  = pkgs.zstd;
              zstd     = pkgs.zstd;
              openssl  = pkgs.openssl;
            };

        mkDevShell = ghcAttr:
          let
            hp = (pkgs.haskell.packages.${ghcAttr}).override {
              overrides = haskellOverlay;
            };
            # Every package the workspace ships, so a single
            # `nix develop` shell can build any of them via
            # `cabal build <pkg>`.
            workspaceDrvs =
              lib.attrValues
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

        packagesForGhc = ghcAttr:
          let
            hp = (pkgs.haskell.packages.${ghcAttr}).override {
              overrides = haskellOverlay;
            };
            perFormat = lib.getAttrs (lib.attrNames wireformPackages) hp;
          in perFormat // {
            wireform = hp.wireform;
          };
      in
      {
        devShells = devShells // {
          default = devShells.${defaultGHC};
        };

        # Every per-format package is also exposed as a build
        # output for downstream Nix consumers. The umbrella
        # `wireform` is the default.
        #
        # Unsuffixed names use defaultGHC (ghc98). CI matrix jobs
        # use <pkg>-<shell> (e.g. wireform-core-ghc96).
        packages =
          let
            defaultPkgs = packagesForGhc defaultGHC;
            matrixPkgs = lib.foldl' (acc: shellName:
              let
                ghcAttr = ghcMatrix.${shellName};
                pkgs_ = packagesForGhc ghcAttr;
                suffixed = lib.mapAttrs'
                  (name: drv: {
                    name = "${name}-${shellName}";
                    value = drv;
                  })
                  pkgs_;
              in acc // suffixed
            ) {} (lib.attrNames ghcMatrix);
          in defaultPkgs // matrixPkgs // {
            default = defaultPkgs.wireform;
          };

        # Buildkite pipeline generators.
        # Preview locally:
        #   nix eval .#ci.main --json --arg changedFiles '["wireform-core/src/Foo.hs"]' | jq .
        #   nix eval .#ci.interop --json --arg changedFiles '["wireform-grpc/src/Foo.hs"]' | jq .
        ci = {
          main = args: import ./nix/ci/pipelines/main.nix ({ inherit lib; } // args);
          interop = args: import ./nix/ci/pipelines/interop.nix ({ inherit lib; } // args);
          packages = import ./nix/ci/packages.nix { inherit lib; };
          dsl = import ./nix/ci/dsl.nix { inherit lib; };
          changes = import ./nix/ci/changes.nix {
            inherit lib;
            packages = import ./nix/ci/packages.nix { inherit lib; };
          };
        };
      }
    );
}

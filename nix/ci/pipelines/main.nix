# Primary CI pipeline: Build -> Test -> Lint + Codegen + Bench + Docs
#
# Invoked with:
#   nix eval .#ci.main --json \
#     --arg changedFiles '["wireform-core/src/Foo.hs"]' \
#     --arg isMain true
{ lib
, changedFiles ? []
, isMain ? false
}:

let
  ci       = import ../dsl.nix { inherit lib; };
  render   = import ../render.nix { inherit lib; };
  packages = import ../packages.nix { inherit lib; };
  changes  = import ../changes.nix { inherit lib packages; };

  affected = changes.affectedPackages changedFiles;
  hasAffected = affected != {};

  defaultGHC = "ghc98";

  inherit (ci) nixAgents;

  # Slug-safe key from package name: wireform-core -> wireform_core
  keyOf = name: builtins.replaceStrings [ "-" ] [ "_" ] name;

  # ------------------------------------------------------------------
  # Build group: nix build every affected package x GHC (flake exposes
  # <pkg>-<shell> outputs; Buildkite interpolates {{matrix.ghc}}). Each
  # package carries its own supported-GHC list (packages.nix), so the
  # matrix only fans out to compilers that can actually build it.
  # ------------------------------------------------------------------
  buildSteps = ci.forPackagesSorted affected (pkg:
    ci.command {
      label = "${pkg.emoji} ${pkg.name} ({{matrix.ghc}})";
      key = "build-${keyOf pkg.name}";
      command = "nix build .#packages.x86_64-linux.${pkg.name}-{{matrix.ghc}} --print-build-logs";
      agents = nixAgents;
      timeout = 45;
      priority = if pkg.tier == "core" then 5 else 0;
      matrix = {
        setup = { ghc = pkg.ghcVersions; };
      };
    }
  );

  buildGroup = ci.group ":nix: Build" {
    key = "build";
    steps = buildSteps;
  };

  # ------------------------------------------------------------------
  # Test group: cabal test for affected packages with test suites
  # ------------------------------------------------------------------
  testable = ci.withTests affected;

  testSteps = ci.forPackagesSorted testable (pkg:
    ci.command {
      label = "${pkg.emoji} ${pkg.name} tests";
      key = "test-${keyOf pkg.name}";
      command = [
        "nix develop .#${defaultGHC} --command cabal test ${pkg.name} --test-show-details=streaming"
        ''buildkite-agent annotate --style success --context "test-${keyOf pkg.name}" --append "| ${pkg.emoji} ${pkg.name} | :white_check_mark: passed |"''
      ];
      agents = nixAgents;
      depends_on = "build-${keyOf pkg.name}";
      timeout = 30;
      artifact_paths = [ "${pkg.path}/dist-newstyle/**/*.log" ];
    }
  );

  # Conformance / codegen self-tests that always run when proto is affected
  conformanceSteps =
    if builtins.hasAttr "wireform-proto" affected then [
      (ci.command {
        label = ":white_check_mark: conformance-self-test";
        key = "test-conformance";
        command = "nix develop .#${defaultGHC} --command cabal run conformance-self-test";
        agents = nixAgents;
        depends_on = "build-wireform_proto";
        timeout = 15;
      })
    ] else [];

  testGroup = ci.group ":test_tube: Test" {
    key = "test";
    depends_on = "build";
    steps = testSteps ++ conformanceSteps;
  };

  # ------------------------------------------------------------------
  # Lint group: HLint + Fourmolu (runs in parallel with tests)
  # ------------------------------------------------------------------
  changedHsFiles =
    builtins.filter (f: lib.hasSuffix ".hs" f) changedFiles;

  lintGroup = ci.group ":mag: Lint" {
    key = "lint";
    depends_on = "build";
    steps = [
      (ci.command {
        label = ":mag: HLint";
        key = "hlint";
        command = "nix develop .#${defaultGHC} --command hlint ${builtins.concatStringsSep " " (map (p: p.path + "/src") (builtins.attrValues affected))}";
        agents = nixAgents;
        soft_fail = true;
        timeout = 15;
      })
      (ci.command {
        label = ":art: Fourmolu";
        key = "fourmolu";
        command = "nix develop .#${defaultGHC} --command fourmolu --mode check ${builtins.concatStringsSep " " (map (p: p.path) (builtins.attrValues affected))}";
        agents = nixAgents;
        soft_fail = true;
        timeout = 10;
      })
    ];
  };

  # ------------------------------------------------------------------
  # Codegen roundtrip group
  # ------------------------------------------------------------------
  codegenSteps = lib.optionals (builtins.hasAttr "wireform-proto" affected) [
    (ci.command {
      label = ":protobuf: gen-wkt verify";
      key = "codegen-wkt";
      command = [
        "nix develop .#${defaultGHC} --command cabal run gen-wkt"
        "git diff --exit-code wireform-proto/src/Proto/Google/Protobuf/"
      ];
      agents = nixAgents;
      depends_on = "build-wireform_proto";
      timeout = 15;
    })
  ] ++ lib.optionals (builtins.hasAttr "wireform-kafka" affected) [
    (ci.command {
      label = ":kafka: regen-kafka-protocol verify";
      key = "codegen-kafka";
      command = [
        "scripts/regen-kafka-protocol.sh /tmp/kafka-schemas"
        "git diff --exit-code wireform-kafka/src/Kafka/Protocol/Generated/"
      ];
      agents = nixAgents;
      depends_on = "build-wireform_kafka";
      timeout = 15;
    })
  ];

  codegenGroup = ci.group ":memo: Codegen Roundtrip" {
    key = "codegen";
    depends_on = "build";
    steps = codegenSteps;
  };

  # ------------------------------------------------------------------
  # Benchmark group (main branch only)
  # ------------------------------------------------------------------
  benchable = ci.withBenchmarks affected;

  benchSteps = ci.forPackagesSorted benchable (pkg:
    ci.command {
      label = ":stopwatch: ${pkg.name} bench";
      key = "bench-${keyOf pkg.name}";
      command = "nix develop .#${defaultGHC} --command cabal bench ${pkg.name} --benchmark-options='--json ${pkg.path}/bench-results.json'";
      agents = nixAgents;
      depends_on = "test";
      timeout = 45;
      artifact_paths = [ "${pkg.path}/bench-results.json" ];
    }
  );

  benchGroup = ci.group ":bar_chart: Benchmarks" {
    key = "bench";
    depends_on = "test";
    if_ = "build.branch == 'main'";
    steps = benchSteps;
  };

  # ------------------------------------------------------------------
  # Docs group (main branch only)
  # ------------------------------------------------------------------
  docsGroup = ci.group ":rocket: Docs" {
    key = "docs";
    depends_on = "test";
    if_ = "build.branch == 'main'";
    steps = [
      (ci.command {
        label = ":book: Haddock";
        key = "haddock";
        command = "nix develop .#${defaultGHC} --command cabal v2-haddock all";
        agents = nixAgents;
        timeout = 30;
        artifact_paths = [ "dist-newstyle/**/doc/html/**/*" ];
      })
    ];
  };

  # ------------------------------------------------------------------
  # Annotation summary step
  # ------------------------------------------------------------------
  summaryStep = ci.command {
    label = ":clipboard: Summary";
    key = "summary";
    depends_on = "test";
    allow_dependency_failure = true;
    command = ''
      echo "### Build Summary" | buildkite-agent annotate --style info --context summary
    '';
    agents = nixAgents;
    timeout = 5;
  };

  # ------------------------------------------------------------------
  # Assemble pipeline
  # ------------------------------------------------------------------
  allSteps =
    (if hasAffected then [ buildGroup ] else [])
    ++ (if testSteps != [] || conformanceSteps != [] then [ testGroup ] else [])
    ++ (if hasAffected then [ lintGroup ] else [])
    ++ (if codegenSteps != [] then [ codegenGroup ] else [])
    ++ (if benchSteps != [] then [ benchGroup ] else [])
    ++ [ docsGroup ]
    ++ [ summaryStep ];

  pipeline = ci.pipeline {
    env = {
      NIX_CONFIG = "experimental-features = nix-command flakes";
    };
    agents = nixAgents;
    steps = allSteps;
  };

in render.renderPipeline pipeline

# Change detection for the wireform monorepo.
#
# Given a list of changed file paths (strings), computes the
# transitive closure of affected packages using the dependency
# graph from packages.nix.
{ lib, packages }:

let
  packageList = builtins.attrValues packages;
  packageNames = builtins.attrNames packages;

  # Sort package paths longest-first so nested paths match before
  # their parents (e.g. wireform-kafka-protocol/ before wireform-kafka/).
  sortedByPathLen = builtins.sort
    (a: b: builtins.stringLength a.path > builtins.stringLength b.path)
    packageList;

  # Map a changed file path to its owning package name, or null.
  fileToPackage = path:
    let
      match = lib.findFirst
        (pkg: lib.hasPrefix "${pkg.path}/" path)
        null
        sortedByPathLen;
    in if match == null then null else match.name;

  # Reverse dependency graph: for each package, the set of packages
  # that directly depend on it.
  reverseDeps =
    let
      addEdge = acc: pkg:
        builtins.foldl'
          (a: dep: a // {
            ${dep} = (a.${dep} or []) ++ [ pkg.name ];
          })
          acc
          pkg.deps;
    in builtins.foldl' addEdge {} packageList;

  # BFS transitive closure over the reverse-dep graph.
  transitiveClosure = directlyChanged:
    let
      go = frontier: seen:
        if frontier == [] then seen
        else
          let
            current = builtins.head frontier;
            rest = builtins.tail frontier;
            rdeps = reverseDeps.${current} or [];
            new = builtins.filter (p: !(builtins.elem p seen)) rdeps;
          in go (rest ++ new) (seen ++ new);
    in go directlyChanged directlyChanged;

  # Paths that trigger a full rebuild of every package.
  globalPaths = [
    "cabal.project"
    "cabal.project.local"
    "flake.nix"
    "flake.lock"
    "nix/"
    ".buildkite/"
  ];

  isGlobalChange = path:
    builtins.any (g: lib.hasPrefix g path) globalPaths;

  # Main entry point: [String] -> packages attrset (subset)
  affectedPackages = changedFiles:
    let
      hitsGlobal = builtins.any isGlobalChange changedFiles;
    in
    if hitsGlobal then packages
    else
      let
        directNames = lib.unique (
          builtins.filter (x: x != null)
            (map fileToPackage changedFiles)
        );
        allNames = transitiveClosure directNames;
      in lib.filterAttrs (n: _: builtins.elem n allNames) packages;

  # Utility: check if a specific package is affected
  isAffected = changedFiles: pkgName:
    builtins.hasAttr pkgName (affectedPackages changedFiles);

in {
  inherit
    fileToPackage
    reverseDeps
    transitiveClosure
    affectedPackages
    isAffected
    isGlobalChange;
}

{ lib }:

let
  # Recursively strip internal `_type` fields and null values,
  # producing a clean attrset that maps 1:1 to Buildkite JSON.

  renderStep = step:
    let t = step._type or "unknown";
    in
      if t == "command"  then renderCommand step
      else if t == "group"   then renderGroup step
      else if t == "wait"    then renderWait step
      else if t == "block"   then renderBlock step
      else if t == "trigger" then renderTrigger step
      else throw "render: unknown step type '${t}'";

  stripInternal = attrs:
    lib.filterAttrs (k: v:
      k != "_type" && v != null && v != [] && v != {}
    ) attrs;

  # Rename "if" back from nix-safe "if_" (already done in dsl.nix
  # via the compact attrset key "if").  Nothing to do here since
  # dsl.nix stores it under the literal key "if".

  renderCommand = step:
    let
      base = stripInternal step;
      # "commands" is the Buildkite key; flatten singleton lists
      commands = step.commands or [];
      command = if builtins.length commands == 1
                then builtins.head commands
                else commands;
      cleaned = builtins.removeAttrs base [ "commands" ]
                // { inherit command; };
    in stripEmpty cleaned;

  renderGroup = step:
    let
      base = stripInternal step;
      renderedSteps = map renderStep (step.steps or []);
    in stripEmpty (base // { steps = renderedSteps; });

  renderWait = step:
    let base = stripInternal step;
    in
    if base == {} || base == { wait = null; }
    then "wait"
    else stripEmpty (builtins.removeAttrs base [ "wait" ]
         // { wait = null; });

  renderBlock = step:
    stripEmpty (stripInternal step);

  renderTrigger = step:
    stripEmpty (stripInternal step);

  stripEmpty = attrs:
    if builtins.isString attrs then attrs
    else lib.filterAttrs (_: v:
      v != null && v != [] && v != {}
    ) attrs;

  # ------------------------------------------------------------------
  # Pipeline-level render
  # ------------------------------------------------------------------

  renderPipeline = pipeline:
    let
      base = stripInternal pipeline;
      renderedSteps = map renderStep (pipeline.steps or []);
    in stripEmpty (base // { steps = renderedSteps; });

in {
  inherit renderStep renderPipeline;
}

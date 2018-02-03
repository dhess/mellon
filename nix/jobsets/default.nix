# Based on
# https://github.com/input-output-hk/iohk-ops/blob/df01a228e559e9a504e2d8c0d18766794d34edea/jobsets/default.nix

{ nixpkgs ? <nixpkgs>
, declInput ? {}
}:

let

  mellonUri = "https://github.com/quixoftic/mellon.git";

  mkFetchGithub = value: {
    inherit value;
    type = "git";
    emailresponsible = false;
  };

  ## nixpkgs-stackage wants a <nixpkgs> path so that it can import
  ## Haskell stuff. Which we use doesn't particularly matter, as
  ## it's only used for importing functions. Here we use a stable
  ## one.
  nixpkgs-src = builtins.fromJSON (builtins.readFile ../nixpkgs-stackage-nixpkgs-src.json);
  nixpkgs-spec = {
    url = "https://github.com/${nixpkgs-src.owner}/${nixpkgs-src.repo}.git";
    rev = "${nixpkgs-src.rev}";
  };

  pkgs = import nixpkgs {};

  defaultSettings = {
    enabled = 1;
    hidden = false;
    keepnr = 10;
    schedulingshares = 100;
    checkinterval = 60;
    enableemail = false;
    emailoverride = "";
    nixexprpath = "nix/jobsets/release.nix";
    nixexprinput = "mellon";
    description = "mellon";
    inputs = {

      ## Note: the nixpkgs input here is for nixpkgs-stackage's
      ## <nixpkgs>. It is not used by hpio.
      nixpkgs = mkFetchGithub "${nixpkgs-spec.url} ${nixpkgs-spec.rev}";

      mellon = mkFetchGithub "${mellonUri} master";

    };
  };

  mkChannelAlt = mellonBranch: nixpkgsRev: nixpkgsStackageRev: {
    inputs = {

      ## Note: the nixpkgs input here is for nixpkgs-stackage's
      ## <nixpkgs>. It is not used by hpio.
      nixpkgs = mkFetchGithub "${nixpkgs-spec.url} ${nixpkgs-spec.rev}";

      nixpkgs_override = mkFetchGithub "https://github.com/NixOS/nixpkgs-channels.git ${nixpkgsRev}";
      nixpkgs_stackage_override = mkFetchGithub "https://github.com/typeable/nixpkgs-stackage.git ${nixpkgsStackageRev}";
      mellon = mkFetchGithub "${mellonUri} ${mellonBranch}";
    };
  };

  mainJobsets = with pkgs.lib; mapAttrs (name: settings: defaultSettings // settings) (rec {
    master = {};
    nixpkgs-unstable = mkChannelAlt "master" "nixpkgs-unstable" "master";
  });

  jobsetsAttrs = mainJobsets;

  jobsetJson = pkgs.writeText "spec.json" (builtins.toJSON jobsetsAttrs);

in {
  jobsets = with pkgs.lib; pkgs.runCommand "spec.json" {} ''
    cat <<EOF
    ${builtins.toJSON declInput}
    EOF
    cp ${jobsetJson} $out
  '';
}

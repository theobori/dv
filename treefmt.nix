{ ... }:
{
  projectRootFile = "flake.nix";
  programs.nixfmt.enable = true;
  programs.actionlint.enable = true;
  programs.mbake.enable = true;
}

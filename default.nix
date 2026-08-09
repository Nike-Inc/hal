# Entry point for building against a given nixpkgs release channel, e.g.:
#   nix-build --arg channel '"unstable"'
#   nix-build                       # defaults to the current stable channel
{ channel ? "26-05" }:

let
  pkgs = import (./nix/nixpkgs + "/${channel}.nix");
in
import ./nix/hal.nix { inherit pkgs; }

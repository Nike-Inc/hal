{ pkgs }:

pkgs.haskellPackages.callCabal2nix "hal" ../. { }

# Stable release channel. Deliberately tracks the moving tip of the
# nixos-26.05 branch rather than an exact commit: that branch only ever
# gains Hydra-vetted backports (security fixes, critical bug fixes) for
# this release, the same stream a real `nix-channel --update` consumes.
import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/heads/nixos-26.05.tar.gz") {}

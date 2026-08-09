# Stable release channel, kept until it ages out of the ~1.5yr support
# window (see nix/nixpkgs/26-05.nix for pinning notes).
import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/heads/nixos-25.05.tar.gz") {}

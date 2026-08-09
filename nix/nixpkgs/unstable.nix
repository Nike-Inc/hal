# Experimental channel. Deliberately unpinned so every CI run picks up whatever
# is at the tip of nixos-unstable.  Marked `experimental: true` in the CI
# matrix so breakage here doesn't block merges.
import (builtins.fetchTarball "https://github.com/NixOS/nixpkgs/archive/refs/heads/nixos-unstable.tar.gz") {}

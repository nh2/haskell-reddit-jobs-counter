# Provides a shell in which development can be done.
# Based on: https://nixos.wiki/wiki/Haskell#Using_shellFor_.28multiple_packages.29
# Run with:
#     nix-shell shell.nix --run $SHELL
#     cabal build
#
# If you have a nixpkgs checkout you want to use, use:
#     NIX_PATH=nixpkgs=/etc/nixos/nixpkgs nix-shell shell.nix --run $SHELL
# If you want to use the known-good nixpkgs version pinned below, use:
#     nix-shell shell.nix --run $SHELL --arg usePinnedNixpkgs true
{
  usePinnedNixpkgs ? false,
  nixpkgs ? builtins.fetchTarball {
    url    = "https://github.com/NixOS/nixpkgs/archive/dfef2e61107dc19c211ead99a5a61374ad8317f4.tar.gz";
    sha256 = "09zps6ih9f5yn4mplfzy90sjxqprjym4cw58fcww8dacddq7gdbg";
  },
  pkgs ? import (if usePinnedNixpkgs then nixpkgs else <nixpkgs>) { config = {}; },  # from https://nixos.wiki/wiki/How_to_fetch_Nixpkgs_with_an_empty_NIX_PATH
}:
let
  lib = pkgs.lib;
  haskellPackages = pkgs.haskellPackages;
in
  (haskellPackages.extend (pkgs.haskell.lib.compose.packageSourceOverrides {
    haskell-reddit-jobs-counter = lib.cleanSource ./.;
    heddit = lib.cleanSource /home/niklas/src/haskell/heddit;
  }))
    # We call on this set `shellFor` to drop us into a shell containing the dependencies of the specified `packages`:
    .shellFor {
      packages = ps: [ ps.haskell-reddit-jobs-counter ];
      buildInputs = [
        pkgs.cabal-install
        haskellPackages.haskell-language-server
      ];
      # withHoogle = true;
    }

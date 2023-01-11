# Wrapper around `shell.nix` to use with file-based environment selectors
# such as the VS Code plugin "Nix Environment Selector".
import ./shell.nix {
  # Using pinned nixpkgs helps with single-instance editors,
  # such as VS Code:
  # When you start `code` and another instance is already open,
  # it will just attach to that one, using its environment variables
  # (including NIX_PATH and thus defining what `<nixpkgs>` refers to in Nix files).
  # Thus, when you want to have multiple editor instances open that use different
  # LSPs from different nixpkgs versions, the only way is to provide nixpkgs
  # explicitly, without `<nixpkgs>`.
  usePinnedNixpkgs = true;
}

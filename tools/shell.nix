let
  inherit (import ./default.nix) pkgs haskell cabal-install ghcid ghcide;
in
  pkgs.mkShell { buildInputs = [ haskell cabal-install ghcid ghcide ]; }

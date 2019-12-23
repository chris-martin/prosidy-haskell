rec {

  ghcVersion = "ghc865";

  versions = import ./versions.nix;

  pkgs = import versions.nixpkgs {};

  inherit (pkgs) callPackage;

  haskellPackages = pkgs.haskell.packages.${ghcVersion};

  haskell = callPackage ./haskell.nix { inherit haskellPackages; };

  inherit (haskellPackages) ghcid;

  ghcide = (import versions.ghcide {})."ghcide-${ghcVersion}";

}

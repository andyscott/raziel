with (import ./nixpkgs.nix {});

haskellPackages.ghcWithPackages (p: with p; [
  aeson
  pipes
  resourcet
  split
  temporary
  xeno
])

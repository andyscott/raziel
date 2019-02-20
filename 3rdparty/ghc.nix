with (import ./nixpkgs.nix {});

haskellPackages.ghcWithPackages (p: with p; [
  exceptions
  split
  #aeson
  #pipes
  #resourcet
  #temporary
  #xeno
])

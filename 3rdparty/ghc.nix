with (import ./nixpkgs.nix {});

haskellPackages.ghcWithPackages (p: with p; [
  exceptions
  split
  proto-lens-protoc
  #aeson
  #pipes
  #resourcet
  #temporary
  #xeno
])

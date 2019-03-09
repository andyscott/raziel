with (import ./nixpkgs.nix {});

haskellPackages.ghcWithPackages (p: with p; [
  exceptions
  proto-lens-protoc
  raw-strings-qq
  split
  tasty
  tasty-hunit
  xml-conduit
  megaparsec
  #aeson
  #pipes
  #resourcet
  #temporary
  #xeno
])

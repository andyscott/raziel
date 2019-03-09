with (import ./nixpkgs.nix {});

haskellPackages.ghcWithPackages (p: with p; [
  attoparsec
  exceptions
  proto-lens-protoc
  raw-strings-qq
  split
  tasty
  tasty-hunit
  xml-conduit
  #aeson
  #pipes
  #resourcet
  #temporary
  #xeno
])

with (import ./nixpkgs.nix {});

haskellPackages.ghcWithPackages (p: with p; [
  exceptions
  fgl
  fgl-visualize
  generic-monoid
  megaparsec
  optparse-applicative
  proto-lens-protoc
  raw-strings-qq
  split
  tasty
  tasty-hunit
  unordered-containers
  xml-conduit
])

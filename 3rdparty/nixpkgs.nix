import (fetchTarball {
  url = https://github.com/NixOS/nixpkgs/archive/db7be5298809304d55bfc563f2685854baf83aea.tar.gz;
  # to calculate sha:
  # nix-prefetch-url --unpack <url>
  sha256 = "1y434vmbyil7bgxk2jgwyprqnsxj645n8jv7nd6xq5jz5wir6nyg";
})

let
  pkgs = import <nixpkgs> {};
  hsPkgs = pkgs.haskell.packages.ghc924;
in
pkgs.mkShell {
  buildInputs = [ pkgs.stack hsPkgs.haskell-language-server hsPkgs.zlib pkgs.zlib.dev ];
  NIX_PATH = "nixpkgs=" + pkgs.path;
}

let
  nixpkgs = fetchTarball "https://github.com/NixOS/nixpkgs/tarball/23.11";
  pkgs = import nixpkgs { config = {}; overlays = []; };
in

pkgs.mkShell rec {
  packages = with pkgs; [
    haskell.compiler.ghc944
    haskellPackages.cabal-fmt
    stylish-haskell
    zlib.dev
    zlib
  ];

  LD_LIBRARY_PATH = pkgs.lib.makeLibraryPath packages;
}

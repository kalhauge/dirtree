{ pkgs ? import (builtins.fetchTarball "https://github.com/nixos/nixpkgs/archive/24c765c744b.tar.gz") {}
, compiler ? "default"
}:
let 
  inherit (import (builtins.fetchTarball "https://github.com/hercules-ci/gitignore/archive/7415c4f.tar.gz") { }) gitignoreSource; 
  hpkgs = if compiler == "default" then pkgs.haskellPackages else pkgs.haskell.packages."${compiler}";
in 
hpkgs.developPackage { 
  root = gitignoreSource ./.;
  name = "dirtree";
  modifier = drv: 
  with pkgs.haskell.lib; 
    addBuildTools drv (with hpkgs; [ cabal-install ghcid ])
  ;
}

{ pkgs ? import ./nix/nixpkgs.nix {}
, compiler ? "default"
}: 
let 
  haskellPackages = 
    if compiler == "default" 
    then pkgs.haskellPackages 
    else pkgs.haskell.packages."${compiler}";
in
  haskellPackages.developPackage {
    root = ./.;
    name = "dirtree";
    source-overrides = {};
    modifier = drv:
      with pkgs.haskell.lib;
      addBuildTools drv (with haskellPackages; [ cabal-install ghcid ])
    ;
  }

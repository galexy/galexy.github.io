{ nixpkgs ? import <nixpkgs> {} }:

let inherit (nixpkgs) pkgs; in 
  pkgs.stdenv.mkDerivation {
    name = "foobarenv";

    buildInputs = [
      pkgs.stack
      pkgs.R
      pkgs.rPackages.ggplot2
      pkgs.rPackages.dplyr
      pkgs.rPackages.ggthemes
      pkgs.rPackages.tidyr
    ];
  }


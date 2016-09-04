{ nixpkgs ? import <nixpkgs> {} }:

let inherit (nixpkgs) pkgs; in 
  pkgs.stdenv.mkDerivation {
    name = "foobarenv";

    buildInputs = [
      pkgs.stack
    ];
  }


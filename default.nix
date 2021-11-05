let
  pkgs = import (builtins.fetchGit {
      url = https://github.com/NixOS/nixpkgs;
      ref = "nixos-21.05";
      rev = "f0869b1a2c0b150aac26e10bb5c2364ffb2e804f";
    }) {};

  hpkgs = pkgs.haskell.packages.ghc8107;

  hal = hpkgs.callCabal2nix "HAL" ./. {};
  hal2-static = pkgs.haskell.lib.justStaticExecutables hal;
  hal-static = pkgs.haskell.lib.overrideCabal hal (old: {
    enableSharedExecutables = false;
    enableSharedLibraries = false;
    configureFlags = [
      "--ghc-option=-optl=-static"
      "--ghc-option=-optl=-L${pkgs.gmp6.override { withStatic = true; }}/lib"
      "--ghc-option=-optl=-L${pkgs.zlib.static}/lib"
      "--ghc-option=-optl=-L${pkgs.glibc.static}/lib"
    ];
  });
in

if pkgs.lib.inNixShell then hpkgs.shellFor {
  packages = p: with hpkgs; [ p.hal p.haskell-language-server ];
  buildInputs = [ pkgs.stack ];
  withHoogle = true;
} else hal

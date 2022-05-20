{ compiler ? "ghc922" }:
let
  sources = import ./nix/sources.nix;
  pkgs = import sources.nixpkgs {};

  gitignore = pkgs.nix-gitignore.gitignoreSourcePure [ ./.gitignore ];

  myHaskellPackages = pkgs.haskell.packages.${compiler}.override {
    overrides = hself: hsuper: {
      "halfsp" =
        hself.callCabal2nix
          "halfsp"
          (gitignore ./.)
          {};
    };
  };

  shell = myHaskellPackages.shellFor {
    packages = p: [
      p."halfsp"
    ];
    buildInputs = [
      myHaskellPackages.haskell-language-server
      pkgs.haskellPackages.cabal-install
      pkgs.haskellPackages.ghcid
      pkgs.haskellPackages.hlint
      pkgs.haskellPackages.hpack
      pkgs.haskellPackages.ormolu
      pkgs.niv
      pkgs.alejandra
    ];
    withHoogle = true;
  };
in
{
  inherit shell;
  inherit myHaskellPackages;
  "halfsp" = myHaskellPackages."halfsp";
}

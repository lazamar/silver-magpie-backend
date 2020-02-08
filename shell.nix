let
    nixpkgs = (import <nixpkgs> {}).fetchFromGitHub {
      owner = "nixOS";
      repo = "nixpkgs-channels";
      rev = "0c960262d159d3a884dadc3d4e4b131557dad116";
      sha256 = "0d7ms4dxbxvd6f8zrgymr6njvka54fppph1mrjjlcan7y0dhi5rb";
    };

    pkgs = import nixpkgs {};
in with pkgs;
  mkShell {
    buildInputs = with pkgs; [
      ghc
      haskellPackages.Cabal
      haskellPackages.cabal-install
      haskellPackages.fast-tags
    ];
  }

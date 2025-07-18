{
  description = "bolt";

  inputs = {
    nixpkgs.url = "github:nixos/nixpkgs/nixos-unstable";
  };

  outputs =
    { self, nixpkgs, ... }:
    let
      supportedSystems = [
        "x86_64-linux"
        "aarch64-darwin"
      ];
      forallSystems =
        f:
        nixpkgs.lib.genAttrs supportedSystems (
          system:
          f (rec {
            inherit system;
            pkgs = nixpkgsFor system;
            haskellPackages = hpkgsFor system pkgs;
          })
        );
      nixpkgsFor =
        system:
        import nixpkgs {
          inherit system;
          config = { };
        };
      hpkgsFor =
        system: pkgs:
        with pkgs.haskell.lib;
        pkgs.haskell.packages.ghc910.override {
          overrides = self: super: {
          };
        };
    in
    {
      packages = forallSystems (
        {
          system,
          pkgs,
          haskellPackages,
        }:
        {
          bolt = haskellPackages.callCabal2nix "bolt" ./. { };
          default = self.packages.${system}.bolt;
        }
      );
      devShells = forallSystems (
        {
          system,
          pkgs,
          haskellPackages,
        }:
        {
          bolt = haskellPackages.shellFor {
            packages = p: [ self.packages.${system}.bolt ];
            buildInputs = with haskellPackages; [
              cabal-install
              haskell-language-server
              # hlint <=9.8 currently
              cabal-gild
              fourmolu
            ];
            withHoogle = true;
          };
          default = self.devShells.${system}.bolt;
        }
      );
    };
}

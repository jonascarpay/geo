{
  description = "geo";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs";
  inputs.flake-utils.url = "github:numtide/flake-utils";

  outputs = inputs:
    let
      overlay = final: prev: {
        haskell = prev.haskell // {
          packageOverrides = hfinal: hprev:
            prev.haskell.packageOverrides hfinal hprev // {
              geo = hfinal.callCabal2nix "geo" ./. { };
            };
        };
        geo = final.haskell.lib.compose.justStaticExecutables final.haskellPackages.geo;
      };
      perSystem = system:
        let
          pkgs = import inputs.nixpkgs { inherit system; overlays = [ overlay ]; };
          hspkgs = pkgs.haskellPackages;
        in
        {
          devShells = rec {
            default = geo-shell;
            geo-shell = hspkgs.shellFor {
              withHoogle = true;
              packages = p: [ p.geo ];
              buildInputs = [
                hspkgs.cabal-install
                hspkgs.haskell-language-server
                hspkgs.hlint
                hspkgs.ormolu
                pkgs.bashInteractive
              ];
            };
          };
          packages = rec {
            default = geo;
            geo = pkgs.geo;
          };
        };
    in
    { inherit overlay; } // inputs.flake-utils.lib.eachDefaultSystem perSystem;
}

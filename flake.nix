{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/release-23.05";
  outputs = { self, nixpkgs, ... }:
    let
      project = [
        "run-persist"
        "website"
        "crowdmatch"
        "shakespeare-sass"
      ];
      hsOverlay = pkgs: self: super:
        builtins.listToAttrs (map (dir: { name = dir; value = self.callCabal2nix dir ./${dir} {}; }) project)
        // builtins.listToAttrs (map (p: { name = p; value =
            pkgs.haskell.lib.unmarkBroken (pkgs.haskell.lib.doJailbreak (super.${p})); }) [
              "postgresql-simple-migration"
              "stripe-core"
              "stripe-http-client"
              "stripe-tests"
            ])
        // builtins.listToAttrs (map (p: { name = p; value =
            pkgs.haskell.lib.unmarkBroken (pkgs.haskell.lib.dontCheck (super.${p})); }) [
              "yesod-gitrev"
            ]);

      myPkgs = import nixpkgs { system = "x86_64-linux"; overlays = [ self.overlays.default ]; };
    in {
      overlays.default = final: prev: {
        myHaskellPackages = prev.haskellPackages.override {
          overrides = hsOverlay final;
        };

        myShell = final.myHaskellPackages.shellFor {
          packages = pkgs: map (x: builtins.getAttr x pkgs) project;
          buildInputs = [ myPkgs.haskell-language-server myPkgs.cachix final.nil ];
        };
      };

      devShells.x86_64-linux.default = myPkgs.myShell;
      #packages.x86_64-linux.default = myPkgs.myHaskellPackages.myPackage;
      #apps.x86_64-linux.default = {
      #  type = "app";
      #  program = myPkgs.lib.getExe self.packages.x86_64-linux.default;
      #};
    };
}

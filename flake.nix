{
  description = "readwise.el flake";

  # outputs = { self, nixpkgs } @ inputs: {
  # 
  #   packages.x86_64-linux.hello = nixpkgs.legacyPackages.x86_64-linux.hello;
  #   packages.x86_64-linux.default = self.packages.x86_64-linux.hello;
  # 
  #   devShells.default = nixpkgs.lib.mkShell {
  #     buildInputs = with nixpkgs; [ python310Packages.flask ];
  #   };
  # 
  # };
  # inputs = {
  #   # this is equivalent to `nixpkgs = { url = "..."; };`
  #   nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  # };

  outputs = inputs@{ self, nixpkgs, flake-parts, ... }:
    flake-parts.lib.mkFlake { inherit inputs; } {
      flake = { };
      systems = [ "aarch64-linux" "x86_64-linux" ];
      perSystem = { system, config, ... }:
        let
          pkgs = import nixpkgs {
            inherit system;
            config.allowAliases = false;
            # overlays = [ self.overlays.default ];
          };
          inherit (pkgs) lib;
          # overlayAttrs = builtins.attrNames (import ./overlays pkgs pkgs);
        in
        {
          devShells.default = pkgs.mkShell {
            buildInputs = with pkgs; [
              python310
              python310Packages.flask
            ];
          };
        };
    };

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";
  inputs.flake-parts.url = "github:hercules-ci/flake-parts";
  inputs.flake-parts.inputs.nixpkgs-lib.follows = "nixpkgs";
}

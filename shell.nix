{ compiler ? "default", doBenchmark ? false }:

let
    nixpkgs =
        let snapshot = builtins.fromJSON (builtins.readFile ./nixpkgs-snapshot.json);
        inherit (snapshot) owner repo rev;
        in builtins.fetchTarball {
            inherit (snapshot) sha256;
            url = "https://github.com/${owner}/${repo}/archive/${rev}.tar.gz";
            };
    pkgs = import nixpkgs { };

    f = import ./default.nix;

    oldHaskellPackages = if compiler == "default"
                       then pkgs.haskellPackages
                       else pkgs.haskell.packages.${compiler};

    haskellPackages = oldHaskellPackages.override {
        overrides = self: super: {
            OpenCL = self.callPackage ../opencl {};
            CLUtil = self.callPackage ../CLUtil {};
            FontyFruity = self.callPackage ../FontyFruity {};
        };
    };

    variant = if doBenchmark then pkgs.haskell.lib.doBenchmark else pkgs.lib.id;

    drv = variant (haskellPackages.callPackage f {});

in

    if pkgs.lib.inNixShell then drv.env else drv

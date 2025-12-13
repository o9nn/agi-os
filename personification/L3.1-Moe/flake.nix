{
  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixpkgs-unstable";

  outputs =
    { self, nixpkgs }:
    let
      inherit (nixpkgs) lib;
      system = "x86_64-linux";
      pkgs = import nixpkgs {
        inherit system;
        config.allowUnfree = true;
        # config.cudaSupport = true;
      };
    in
    {
      devShells.${system}.default = pkgs.mkShell {
        venvDir = ".venv";

        env = {
          CUDA_PATH = pkgs.cudatoolkit;
          EXTRA_LDFLAGS = "-L/lib -L${pkgs.linuxPackages.nvidia_x11}/lib";
          EXTRA_CCFLAGS = "-I/usr/include";
          LD_LIBRARY_PATH = lib.makeLibraryPath [ pkgs.stdenv.cc.cc.lib ];
        };

        packages =
          with pkgs;
          [ python312 ]
          ++ (with pkgs.python312Packages; [
            pip
            venvShellHook
            torchWithCuda
            # torchvision
            # torchaudio
          ]);
      };
    };
}

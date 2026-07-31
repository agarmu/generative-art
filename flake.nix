{
  description = "Generative art — Processing + Scala";

  inputs.nixpkgs.url = "github:NixOS/nixpkgs/nixos-unstable";

  outputs =
    { nixpkgs, ... }:
    let
      lib = nixpkgs.lib;
      forAllSystems = lib.genAttrs [
        "x86_64-linux"
        "aarch64-linux"
      ];
    in
    {
      devShells = forAllSystems (
        system:
        let
          pkgs = nixpkgs.legacyPackages.${system};
          jdk = pkgs.jdk17;

          processingJars = pkgs.runCommand "processing-jars" { src = pkgs.processing; } ''
            shopt -s nullglob
            jars=( "$src"/lib/app/*.jar )
            if [[ ''${#jars[@]} -eq 0 ]]; then
              echo "error: no jars found in $src/lib/app"
              exit 1
            fi
            mkdir -p $out
            cp "''${jars[@]}" $out/
          '';
          minimalPackages = with pkgs; [
            jdk
            processingJars
            (pkgs.scala.override { jre = jdk; })
            (pkgs.sbt.override { jre = jdk; })
          ];
        in
        {
          default = pkgs.mkShell {
            packages = minimalPackages ++ [
              (pkgs.metals.override { jre = jdk; })
              (pkgs.scalafmt.override { jre = jdk; })
            ];

            shellHook = ''
              export PROCESSING_LIB_DIR="${processingJars}"
            '';
          };

          minimal = pkgs.mkShell {
            packages = minimalPackages;
            shellHook = ''
              export PROCESSING_LIB_DIR="${processingJars}"
            '';
          };
        }
      );
    };
}

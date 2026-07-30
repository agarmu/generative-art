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
        in
        {
          default = pkgs.mkShell {
            packages = [
              jdk
              processingJars
              (pkgs.sbt.override { jre = jdk; })
              (pkgs.scala.override { jre = jdk; })
              (pkgs.metals.override { jre = jdk; })
              (pkgs.scalafmt.override { jre = jdk; })
	      pkgs.scalafmt 
            ];

            shellHook = ''
              export PROCESSING_LIB_DIR="${processingJars}"
            '';
          };
        }
      );
    };
}

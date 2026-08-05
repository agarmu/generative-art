{ pkgs ? import <nixpkgs> { } }:

assert !pkgs.stdenv.hostPlatform.isDarwin || pkgs.stdenv.hostPlatform.isAarch64;

let
  jdk = pkgs.jdk17;

  processingSource =
    if pkgs.stdenv.hostPlatform.isDarwin then
      pkgs.fetchurl {
        url = "https://github.com/processing/processing4/releases/download/processing-1434-4.5.6/processing-4.5.6-macos-aarch64-portable.zip";
        sha256 = "sha256-rIJOvPJeJtS6bTCm/NutI6ZeX9o9Ug7ALpzd8pqKNlY=";
      }
    else
      pkgs.processing;
  processingJarPath =
    if pkgs.stdenv.hostPlatform.isDarwin then
      "Processing.app/Contents/app/resources/modes/java/mode"
    else
      "lib/app";
  processingJars = pkgs.runCommand "processing-jars" {
    src = processingSource;
    nativeBuildInputs = pkgs.lib.optionals pkgs.stdenv.hostPlatform.isDarwin [ pkgs.unzip ];
  } ''
    ${pkgs.lib.optionalString pkgs.stdenv.hostPlatform.isDarwin ''
      unzip -q "$src" -d unpacked
      jarDir="unpacked/${processingJarPath}"
    ''}
    ${pkgs.lib.optionalString (!pkgs.stdenv.hostPlatform.isDarwin) ''
      jarDir="$src/${processingJarPath}"
    ''}
    shopt -s nullglob
    jars=( "$jarDir"/*.jar )
    if [[ ''${#jars[@]} -eq 0 ]]; then
      echo "error: no jars found in $jarDir"
      exit 1
    fi
    mkdir -p $out
    cp "''${jars[@]}" $out/
  '';
  minimalPackages = with pkgs; [
    jdk
    bun
    processingJars
    (pkgs.scala.override { jre = jdk; })
    (pkgs.sbt.override { jre = jdk; })
  ];
in
pkgs.mkShell {
  packages = minimalPackages ++ [
    (pkgs.metals.override { jre = jdk; })
    (pkgs.scalafmt.override { jre = jdk; })
  ];

  shellHook = ''
    export PROCESSING_LIB_DIR="${processingJars}"
  '';
}


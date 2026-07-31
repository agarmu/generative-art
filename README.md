# processing-scala

Minimal Processing 4 + Scala 3 setup for generative art, managed with Nix flakes.

## Quickstart

```sh
nix develop
sbt run
```

`nix develop` imports dependencies and sets `PROCESSING_LIB_DIR` (required by `build.sbt`). `sbt run` compiles and opens the sketch.

## Environment

All tools are pinned to **jdk17** to match the Processing 4 package in nixpkgs, which (as of this writing)
hardcodes that version at build time.

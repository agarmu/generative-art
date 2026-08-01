name := "genart"
version := "0.1"
scalaVersion := "3.3.6"

val processingLibDir = file(
  sys.env.getOrElse(
    "PROCESSING_LIB_DIR",
    sys.error("PROCESSING_LIB_DIR not set! run: nix-shell")
  )
)

Compile / unmanagedBase := {
  val hasProcessingJars =
    Option(processingLibDir.listFiles())
      .exists(_.exists(entry => entry.isFile && entry.getName.endsWith(".jar")))
  if (!processingLibDir.isDirectory || !hasProcessingJars)
    sys.error(
      s"No Processing jars found in PROCESSING_LIB_DIR=$processingLibDir"
    )
  processingLibDir
}

fork := true
run / javaOptions ++= Seq(
  s"-Djava.library.path=$processingLibDir",
  s"-Djna.library.path=$processingLibDir"
)

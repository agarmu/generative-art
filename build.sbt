name := "genart"
version := "0.1"
scalaVersion := "3.3.6"

val processingLibDir = sys.env.getOrElse(
  "PROCESSING_LIB_DIR",
  sys.error("PROCESSING_LIB_DIR not set! run: nix develop")
)

Compile / unmanagedJars ++= {
  val jars = (file(processingLibDir) * "*.jar").get
  if (jars.isEmpty)
    sys.error(s"No jars found in PROCESSING_LIB_DIR=$processingLibDir")
  jars.classpath
}

fork := true
run / javaOptions ++= Seq(
  s"-Djava.library.path=$processingLibDir",
  s"-Djna.library.path=$processingLibDir"
)

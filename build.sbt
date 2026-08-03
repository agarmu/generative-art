ThisBuild / version := "0.1"
ThisBuild / scalaVersion := "3.3.6"

def processingLibDir: File = {
  val directory = file(
    sys.env.getOrElse(
      "PROCESSING_LIB_DIR",
      sys.error("PROCESSING_LIB_DIR not set! run: nix-shell")
    )
  )
  require(directory.isDirectory, s"Processing directory not found: $directory")
  require(
    (directory * "*.jar").get().nonEmpty,
    s"No Processing jars found in $directory"
  )
  directory
}

lazy val testSettings = Seq(
  libraryDependencies += "org.scalameta" %% "munit" % "1.3.6" % Test,
  Test / fork := true
)

lazy val processingSettings = Seq(
  Compile / unmanagedJars ++= {
    implicit val converter: xsbti.FileConverter = fileConverter.value
    (processingLibDir * "*.jar").classpath
  },
  Compile / fork := true,
  Compile / javaOptions ++= Seq(
    s"-Djava.library.path=$processingLibDir",
    s"-Djna.library.path=$processingLibDir"
  )
)

def portableSources(directory: File): Seq[File] =
  ((directory ** "*.scala") --- (directory / "processing" ** "*.scala")).get()

lazy val genart = project
  .in(file("target/projects/genart"))
  .settings(testSettings)
  .settings(
    Compile / unmanagedSources := portableSources(
      (LocalRootProject / baseDirectory).value / "src/main/scala/genart"
    ),
    Test / unmanagedSources := portableSources(
      (LocalRootProject / baseDirectory).value / "src/test/scala/genart"
    )
  )

lazy val coreJVM = project
  .in(file("target/projects/coreJVM"))
  .dependsOn(genart)
  .settings(processingSettings)
  .settings(testSettings)
  .settings(
    Compile / unmanagedSourceDirectories := Seq(
      (LocalRootProject / baseDirectory).value / "src/main/scala/genart/processing"
    ),
    Test / unmanagedSourceDirectories := Seq(
      (LocalRootProject / baseDirectory).value / "src/test/scala/genart/processing"
    ),
    Test / javaOptions += "-Djava.awt.headless=true"
  )

lazy val root = project
  .in(file("."))
  .aggregate(genart, coreJVM)
  .dependsOn(coreJVM)
  .settings(processingSettings)
  .settings(
    name := "genart-desktop",
    Compile / unmanagedSources := Seq(
      baseDirectory.value / "src/main/scala/Sketch.scala"
    ),
    Test / unmanagedSources := Seq.empty
  )

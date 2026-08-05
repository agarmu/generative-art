import org.scalajs.jsenv.nodejs.NodeJSEnv

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

def sourceSettings(path: String, excluded: String*) =
  Seq(Compile -> "main", Test -> "test").flatMap { case (config, scope) =>
    inConfig(config)(
      Seq(
        unmanagedSourceDirectories := Seq(
          (LocalRootProject / baseDirectory).value / "src" / scope / "scala" / path
        ),
        unmanagedSources := unmanagedSourceDirectories.value.flatMap { root =>
          excluded
            .foldLeft(root ** "*.scala") { (sources, path) =>
              sources --- (root / path).allPaths
            }
            .get()
        }
      )
    )
  }

lazy val portableSettings =
  sourceSettings("genart", "processing", "p5") ++ testSettings

lazy val bunSettings = Seq(
  Test / fork := false,
  scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.ESModule)),
  Test / scalaJSLinkerConfig ~= (_.withModuleKind(ModuleKind.CommonJSModule)),
  jsEnv := Def.uncached {
    new NodeJSEnv(
      NodeJSEnv
        .Config()
        .withExecutable("bun")
        .withArgs(List("run", "--no-install", "-"))
    )
  }
)

lazy val genart = project
  .in(file("target/projects/genart"))
  .settings(portableSettings)

lazy val coreJVM = project
  .in(file("target/projects/coreJVM"))
  .dependsOn(genart)
  .settings(
    sourceSettings("genart/processing"),
    testSettings,
    processingSettings,
    Test / javaOptions += "-Djava.awt.headless=true"
  )

lazy val genartJS = project
  .in(file("target/projects/genartJS"))
  .enablePlugins(ScalaJSPlugin)
  .settings(portableSettings, bunSettings)

lazy val coreJS = project
  .in(file("target/projects/coreJS"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(genartJS)
  .settings(
    sourceSettings("genart/p5"),
    bunSettings,
    libraryDependencies += "org.scala-js" %% "scalajs-dom" % "2.8.1"
  )

lazy val browserExamples = project
  .in(file("target/projects/browserExamples"))
  .enablePlugins(ScalaJSPlugin)
  .dependsOn(coreJS)
  .settings(
    sourceSettings("examples", "ProcessingExampleRunner.scala"),
    bunSettings,
    publish / skip := true,
    Test / unmanagedSourceDirectories := Seq.empty,
    Compile / fastLinkJS / scalaJSLinkerOutputDirectory :=
      (LocalRootProject / baseDirectory).value / "target/browser"
  )

lazy val root = project
  .in(file("."))
  .aggregate(genart, coreJVM, genartJS, coreJS)
  .dependsOn(coreJVM)
  .settings(
    sourceSettings("examples", "BrowserExampleRunner.scala"),
    processingSettings,
    name := "genart-desktop",
    Test / unmanagedSourceDirectories := Seq.empty
  )

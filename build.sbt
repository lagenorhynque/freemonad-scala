lazy val root = (project in file("."))
  .settings(
    name := "freemonad-scala",
    version := "0.1.0",
    scalaVersion := "2.12.6",
    scalacOptions ++= Seq("-deprecation", "-feature", "-unchecked", "-Xlint"),
    libraryDependencies ++= Seq(
      "org.scalaz" %% "scalaz-core" % "7.2.22",
      "org.scalatest" %% "scalatest" % "3.0.5" % "test",
      "org.scalamock" %% "scalamock-scalatest-support" % "3.6.0" % "test"
    )
  )

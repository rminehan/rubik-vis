lazy val root = project
  .in(file("."))
  .settings(
    name := "rubik-vis",
    description := "Visualiser for solving rubik's cubes",
    version := "1.0",

    scalaVersion := "3.2.0",

    scalacOptions ++= List("-deprecation"),
  )

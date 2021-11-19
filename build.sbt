
enablePlugins(Antlr4Plugin)

ThisBuild / organization  := "edu.vtc"
ThisBuild / version       := "0.1.0-SNAPSHOT"
ThisBuild / scalaVersion  := "2.13.7"
ThisBuild / scalacOptions :=
  Seq("-encoding", "UTF-8",
      "-feature",
      "-deprecation",
      "-unchecked",
      "-Wunused:nowarn",
      "-Xsource:3",
      "-Ywarn-dead-code",
      "-Ywarn-value-discard")

libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.10"
libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.10" % "test"

logBuffered in Test := false

lazy val merc = (project in file("."))
  .settings(
    name := "Merc",

    Antlr4 / antlr4Version     := "4.9.2",
    Antlr4 / antlr4PackageName := Some("edu.vtc.merc"),
    Antlr4 / antlr4GenListener := true,
    Antlr4 / antlr4GenVisitor  := true
  )

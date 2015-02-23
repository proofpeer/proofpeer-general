lazy val root = project.in(file(".")).aggregate(fooJS, fooJVM).
  settings(
    publish := {},
    publishLocal := {}
  )

lazy val foo = crossProject.in(file(".")).
  settings(
    name := "ProofPeer General",
    organization := "net.proofpeer",
    version := "0.1-SNAPSHOT",
    scalaVersion := "2.11.5",
    scalacOptions += "-deprecation"
  ).
  jvmSettings(
    libraryDependencies += "org.scalacheck" %% "scalacheck" % "1.10.1" % "test"
  )

lazy val fooJS = foo.js
lazy val fooJVM = foo.jvm
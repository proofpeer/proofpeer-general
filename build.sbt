name := "ProofPeer General"

version := "0.1-SNAPSHOT"

scalaVersion := "2.11.2"

lazy val root = project.in(file(".")).aggregate(rootJS, rootJVM)

lazy val rootJS = project.in(file("proofpeer-general-js")).settings(scalaJSSettings: _*).settings(
  name := "proofpeer-general",  
  scalaVersion := "2.11.2",
  unmanagedSourceDirectories in Compile +=
    (baseDirectory.value / "..") / "proofpeer-general-shared" / "src" / "main" / "scala"
)

lazy val rootJVM = project.in(file("proofpeer-general-jvm")).settings(
  name := "proofpeer-general",
  scalaVersion := "2.11.2",
  unmanagedSourceDirectories in Compile +=
    (baseDirectory.value / "..") / "proofpeer-general-shared" / "src" / "main" / "scala"
)

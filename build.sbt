name := "mintpresso"

description :=  "mintpresso Scala API Pack"

version := "0.1"

scalaVersion := "2.9.2"

libraryDependencies ++= Seq(
  "net.databinder.dispatch" %% "dispatch-core" % "0.9.5",
  "org.specs2" %% "specs2" % "1.12.3" % "test"
)

resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)
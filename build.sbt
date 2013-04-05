name := "mintpresso"

description :=  "mintpresso Scala API Pack"

version := "0.1.2"

scalaVersion := "2.10.0"

publishTo := Some(Resolver.file("file",  new File(Path.userHome.absolutePath+"/.m2/repository")))

libraryDependencies ++= Seq(
  "net.databinder.dispatch" % "dispatch-core_2.10" % "0.9.5",
  "org.specs2" % "specs2_2.10" % "1.14",
  "net.liftweb" % "lift-json_2.10" % "2.5-RC2"
)

resolvers ++= Seq(
  "snapshots" at "http://oss.sonatype.org/content/repositories/snapshots",
  "releases"  at "http://oss.sonatype.org/content/repositories/releases"
)
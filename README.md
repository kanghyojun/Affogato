# Affogato
## MINTPRESSO API for Scala
It supports basic graph manipulation and data querying for MINTPRESSO Data Cloud.

## Getting Started
Add this repository as a project dependency to your sbt project.
```scala
import sbt._

object MyBuild extends Build {
  lazy val root = Project( ... ) dependsOn (
    RootProject(uri("git://github.com/admire93/Affogato.git"))
    // can use local repo uri("file:////Users/eces/affogato")
  )
}
```

Now you can use **Affogato** by adding it to Project dependencies.
```scala
"com.mintpresso" %% "mintpresso" % "0.1.9"
```


If you're using Play Framework 2, edit `project/Build.scala`.
```scala
  val appDependencies = Seq(
    // Add your project dependencies here
    "com.mintpresso" %% "mintpresso" % "0.1.9"
  )


  val main = play.Project(appName, appVersion, appDependencies).settings(
    // Add your own project settings here      
  ).dependsOn(
    RootProject(uri("git://github.com/admire93/affogato.git"))
  )
```

## Advanced Use
Clone this repository first.
```bash
$ git clone https://github.com/admire93/Affogato.git affogato

```

You can edit `affogato.conf` for your environment before `sbt public-local`.
```bash
$ cd src/main/resources/affogato.conf
$ cat affogato.conf

mintpresso.protocol=http
mintpresso.host="api.mintpresso.com"
mintpresso.port=80
mintpresso.version=v1
```

Build and publish to an local repository(~/.ivy2).
```bash
$ cd affogato
$ sbt publish-local
```
Generated documentation will be placed in `target/scala-2.10/api`. Type`open target/scala-2.10/api/index.html` to see.

And then, add a custom resolver to your sbt project.
```scala
resolvers += "Local Repository" at "file://"+Path.userHome.absolutePath+"/.ivy2/local"
```

Also on sbt `Project` dependencies.
```scala
"com.mintpresso" %% "mintpresso" % "0.1-SNAPSHOT"
```

## Examples
See [Affogato Scala Doc](http://docs.mintpresso.com/affogato).

## Contributors
@admire93 @eces

## Further Information
You can visit [MINTPRESSO 민트프레소](http://mintpresso.com) official website or go to read more [Examples, User guide & API documentation](http://docs.mintpresso.com).

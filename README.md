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

### Play Framework 2
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

We can export your API key to `conf/application.conf`. This configuration makes you feel happy when you deploy on several servers or use multiple keys.
```json
# one key and id pair
mintpresso.api=YOUR_API_KEY_HERE
mintpresso.id=1

# many pairs
mintpresso {
  internal {
    api=API_KEY_FOR_SECURED_OPERATION
    id=1000
  }
  external {
    api=API_KEY_FOR_READONLY
    id=1000
  }
}
```


Initialze affogato variable in any `Controller` or `Model` code.
```scala
val mintpresso: Affogato = Affogato( 
  Play.configuration.getString("mintpresso.api").getOrElse(""),
  Play.configuration.getLong("mintpresso.id").getOrElse(0L)
)
/*
  It works but not DRY.
  val mintpresso: Affogato = Affogato( "YOUR_API_KEY_HERE", 0 )
*/
```
Creating wrapper class like `AffogatoController` is somehow useful.

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

### Using in console
Start console in working directory.
```bash
~/affogato$ sbt console
...
scala> 
```

Import a package of Affogato and put your API KEY. See your settings first at **[Panel](http://mintpresso.com/login) > Overview > API Setting**. 
```scala
scala> import com.mintpresso._
import com.mintpresso._

scala> val mintpresso: Affogato = Affogato("YOUR_API_KEY_HERE", 1)
mintpresso: com.mintpresso.Affogato = Affogato(YOUR_API_KEY_HERE, 1)

scala> mintpresso.get(1)
res0: Option[com.mintpresso.Point] = Some(Point(...))
```
Now you can play with MINTPRESSO.

### Using in project
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

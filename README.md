# MINTPRESSO API for Scala
It supports basic graph manipulation and data querying for MINTPRESSO Data Cloud.

# Getting Started
Clone this repository first and build to an local repository(ivy2).
```bash
$ git clone https://github.com/admire93/Affogato.git affogato

$ cd affogato
$ sbt publish-local
```

You can edit `affogato.conf` for your environment before `sbt public-local`.
```
$ cd src/main/resources/affogato.conf
$ cat affogato.conf

mintpresso.protocol=http
mintpresso.host="api.mintpresso.com"
mintpresso.port=9001
mintpresso.version=v1
```

Now you can use **Affogato** by adding to sbt dependencies
```scala
"com.mintpresso" %% "mintpresso" % "0.1.9"
```

# Examples
> TODO

# Contributors
@admire93

# Further Information
You can visit [MINTPRESSO 민트프레소](http://mintpresso.com) official website or go to read more [Examples, User guide & API documentation](http://docs.mintpresso.com).

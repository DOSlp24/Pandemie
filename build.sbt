name := "PandemiePlay"
 
version := "1.0" 
      
lazy val `pandemieplay` = (project in file(".")).enablePlugins(PlayScala)

resolvers += "scalaz-bintray" at "https://dl.bintray.com/scalaz/releases"
      
resolvers += "Akka Snapshot Repository" at "http://repo.akka.io/snapshots/"
      
scalaVersion := "2.12.2"

libraryDependencies ++= Seq( jdbc , ehcache , ws , specs2 % Test , guice )

unmanagedResourceDirectories in Test <+=  baseDirectory ( _ /"target/web/public/test" )

libraryDependencies ++= Seq("com.typesafe.akka" %% "akka-http" % "10.1.8", "com.typesafe.akka" %% "akka-http-core" % "10.1.8", "com.typesafe.akka" %% "akka-stream" % "2.5.23", "com.typesafe.akka" %% "akka-http-xml" % "10.1.10")
libraryDependencies += "com.typesafe.akka" %% "akka-http-spray-json" % "10.1.10"
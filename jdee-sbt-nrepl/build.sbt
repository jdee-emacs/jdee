// JDEE repl plugin using sbt to find classpath and such

sbtPlugin := true

name := "sbt-nrepl"

organization := "org.jdee"
organizationName := "Java Development Environment for Emacs"
organizationHomepage := Some(url("https://jdee.sourceforge.net"))

// set the main class for 'sbt run'
//mainClass in (Compile, run) := Some("clojure.main")

// set the main class for packaging the main jar
//mainClass in (Compile, packageBin) := Some("clojure.main")

libraryDependencies ++= Seq(
   "jdee" % "jdee-nrepl" % "0.1.0-SNAPSHOT"
   )

resolvers += Resolver.mavenLocal
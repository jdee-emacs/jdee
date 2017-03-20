package org.jdee.nrepl.sbt

import sbt._
import Keys._

object JdeeSbtPlugin extends AutoPlugin {
       override def globalSettings: Seq[Setting[_]] = super.globalSettings ++ Seq(
       Keys.commands += startRepl
       )

       object autoImport {
       val dummy = settingKey[String]("Dummy")
       }
       import autoImport._;

//       override lazy val projectSettings = Seq(commands += startRepl)
//       override lazy val settings = Seq(commands += startRepl)
       lazy val startRepl =
            Command.command("start") {
                (state: State) =>
                  println("SBT nrepl starting")

                  // Options, including the data we need to send back
                  fork in myRunTask := true
                  val sourceRoutes = "-Djdee.sourceRoots=" + {javaSource in Compile}
                  val testRoutes = "-Djdee.testRootes=" + {javaSource in Test}
                  javaOptions in myRunTask ++= Seq(
                              sourceRoutes,
                              testRoutes
                              )


//                  mainClass in (Compile, run) = Some("clojure.main")

                  fullRunTask(myRunTask, Compile, "clojure.main")
                  state
                  }

                  lazy val myRunTask = taskKey[Unit]("NREPL run task")



}
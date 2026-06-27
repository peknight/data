import com.peknight.build.gav.*
import com.peknight.build.sbt.*

commonSettings

lazy val data = (project in file("."))
  .settings(name := "data")
  .aggregate(dataCore.projectRefs *)

lazy val dataCore = (projectMatrix in file("data-core"))
  .settings(name := "data-core")
  .settings(libraryDependencies ++= dependencies(peknight.codec))
  .jvmPlatform(scalaVersions = Seq(scala.scala3.version))
  .jsPlatform(scalaVersions = Seq(scala.scala3.version))

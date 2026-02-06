import com.peknight.build.gav.*
import com.peknight.build.sbt.*

commonSettings

lazy val data = (project in file("."))
  .settings(name := "data")
  .aggregate(
    dataCore.jvm,
    dataCore.js,
  )

lazy val dataCore = (crossProject(JVMPlatform, JSPlatform) in file("data-core"))
  .settings(name := "data-core")
  .settings(crossDependencies(
    peknight.codec,
  ))

import com.peknight.build.gav.*
import com.peknight.build.sbt.*

commonSettings

lazy val data = (project in file("."))
  .settings(name := "data")
  .aggregate(
    dataCore.jvm,
    dataCore.js,
    dataCore.native,
  )

lazy val dataCore = (crossProject(JVMPlatform, JSPlatform, NativePlatform) in file("data-core"))
  .settings(name := "data-core")
  .settings(crossDependencies(typelevel.cats))

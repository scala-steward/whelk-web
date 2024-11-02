enablePlugins(ScalaJSPlugin)

scalaJSUseMainModuleInitializer := true

organization := "org.geneontology"

name := "whelk-web"

version := "0.1"

licenses := Seq("BSD-3-Clause" -> url("https://opensource.org/licenses/BSD-3-Clause"))

scalaVersion := "2.13.15"

scalacOptions := Seq("-unchecked", "-deprecation", "-encoding", "utf8", "-Ypatmat-exhaust-depth", "off")

libraryDependencies ++= {
  Seq(
    "com.raquo" %%% "laminar" % "0.12.2",
    "org.geneontology" %%% "whelk" % "1.2.1"
  )
}

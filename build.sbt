lazy val moduleslib = project.in(file("."))
  .settings(
	  name := "ModulesLib"
	  , organization := "just4fun"
	  , version := "1.0-SNAPSHOT"
	  , scalaVersion := "2.11.7"
	  , scalacOptions += "-Xexperimental"
	  , licenses := Seq("BSD-style" -> url("http://www.opensource.org/licenses/bsd-license.php"))
	  , homepage := Some(url("https://github.com/just-4-fun"))
  )
  .settings(dependencies: _*)

lazy val dependencies = Seq(
	libraryDependencies += "just4fun" %% "logger" % "1.0-SNAPSHOT"
	//	, libraryDependencies += "just4fun" %% "utils" % "1.0-SNAPSHOT"
)

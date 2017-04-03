enablePlugins(ScalaJSPlugin)

name := "Mandelbrot Set Representation"
scalaVersion := "2.12.1"

// This is an application with a main method
scalaJSUseMainModuleInitializer := true

libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.1"
)

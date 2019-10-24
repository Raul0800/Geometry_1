name := "ex"

version := "0.1"

scalaVersion := "2.12.9"

val Http4sVersion = "0.20.8"
val CirceVersion = "0.11.1"

//libraryDependencies ++= Seq(
//  "org.http4s"      %% "http4s-blaze-server" % Http4sVersion,
//  "org.http4s"      %% "http4s-blaze-client" % Http4sVersion,
//  "org.http4s"      %% "http4s-circe"        % Http4sVersion,
//  "org.http4s"      %% "http4s-dsl"          % Http4sVersion,
//  "io.circe"        %% "circe-generic"       % CirceVersion,
//  "org.http4s"      %% "http4s-circe"        % Http4sVersion,
//  "io.circe"        %% "circe-generic"       % CirceVersion,
//  "io.circe"        %% "circe-literal"       % CirceVersion,
//  "org.json4s"      %% "json4s-jackson"      % "3.2.11",
//  "org.http4s"      %% "http4s-json4s-jackson" % Http4sVersion,
//  "org.json4s"      %% "json4s-native"        % "3.2.11")
libraryDependencies += "net.liftweb" % "lift-sbt" % "2.4"
libraryDependencies += "net.liftweb" %% "lift-json" % "3.1.0-M1"
libraryDependencies += "org.typelevel" %% "cats-core" % "2.0.0"

scalacOptions += "-Ypartial-unification"
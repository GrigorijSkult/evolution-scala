name := "evo-scala-bootcamp-homework"

version := "0.1"

scalaVersion := "2.13.3"

val doobieVersion = "0.9.0"
val catsVersion = "2.2.0"
val catsScalacheckVersion = "0.2.0"

val akkaVersion = "2.6.9"

libraryDependencies ++= Seq(
  "org.typelevel" %% "cats-core" % catsVersion,
  "io.chrisdavenport" %% "cats-scalacheck" % catsScalacheckVersion % Test,
  "org.scalatestplus" %% "scalatestplus-scalacheck" % "3.1.0.0-RC2" % Test,
)


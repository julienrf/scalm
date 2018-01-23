import sbt.Credentials

enablePlugins(ScalaJSBundlerPlugin)
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.3")

name := "scalm"
organization := "org.julienrf"

npmDependencies in Compile += "snabbdom" -> "0.6.7"
libraryDependencies ++= Seq(
  "org.scala-js" %%% "scalajs-dom" % "0.9.1",
  "org.typelevel" %%% "cats" % "0.9.0"
)

scalaVersion := "2.12.4"
scalacOptions ++= Seq(
  "-feature",
  "-deprecation",
  "-encoding", "UTF-8",
  "-unchecked",
  "-Xlint",
  "-Yno-adapted-args",
  "-Ywarn-dead-code",
  "-Ywarn-numeric-widen",
  "-Ywarn-value-discard"
)

publishTo := sonatypePublishTo.value

inThisBuild(List(
  licenses := Seq("BSD-3-Clause" -> url("http://opensource.org/licenses/BSD-3-Clause")),
  homepage := Some(url("https://github.com/julienrf/scalm")),
  developers := List(Developer("julienrf", "Julien Richard-Foy", "julien@richard-foy.fr", url("http://julien.richard-foy.fr"))),
  scmInfo := Some(ScmInfo(url("https://github.com/julienrf/scalm"), "scm:git:git@github.com:julienrf/scalm.git")),
  pgpPublicRing := file("./travis/local.pubring.asc"),
  pgpSecretRing := file("./travis/local.secring.asc"),
  pgpPassphrase := sys.env.get("PGP_PASS").map(_.toArray),
  credentials ++= (
    for {
      username <- sys.env.get("SONATYPE_USER")
      password <- sys.env.get("SONATYPE_PASSWORD")
    } yield Credentials("Sonatype Nexus Repository Manager", "oss.sonatype.org", username, password)
    ).toList
))

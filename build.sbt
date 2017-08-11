lazy val commonSettings = Seq(

version := "0.0.1",
scalaVersion := "2.11.8",
EclipseKeys.withSource := true,
parallelExecution in test := false,
test in assembly := {},
assemblyMergeStrategy in assembly := {
 case PathList("META-INF", xs @ _*) => MergeStrategy.discard
 case x => MergeStrategy.first
}
) ++ packAutoSettings

lazy val project = Project(
id = "bayesian",
base = file(".")).settings(commonSettings).settings(
name := "bayesian",

libraryDependencies ++= Seq(
  "org.json4s" %% "json4s-jackson" % "3.2.11")
)

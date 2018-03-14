import com.typesafe.sbt.SbtScalariform.ScalariformKeys
import scalariform.formatter.preferences._

lazy val root = (project in file(".")).
  settings(
    inThisBuild(List(
      organization    := "araspitzu",
      scalaVersion    := "2.12.4",
      version         := "0.0.1",
      mainClass in Compile := Some("example.AppEntryPoint"),
      ScalariformKeys.preferences := scalariformPref.value
    )),
    name := "liability_proof",
    libraryDependencies ++= Seq(
      "org.json4s" %% "json4s-native" % "3.5.3",
      "org.json4s" %% "json4s-jackson" % "3.5.3",
      "org.scalatest" %% "scalatest" % "3.0.4" % "test"
    )
  )

enablePlugins(JavaAppPackaging)

lazy val scalariformPref = Def.setting {
  ScalariformKeys.preferences.value
    .setPreference(AlignSingleLineCaseStatements, true)
    .setPreference(DanglingCloseParenthesis, Preserve)
    .setPreference(CompactStringConcatenation, true)
}

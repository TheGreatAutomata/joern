name := "semanticcpg"

libraryDependencies ++= Seq(
  "io.shiftleft"         %% "codepropertygraph" % Versions.cpg,
  "com.michaelpollmeier" %% "scala-repl-pp"     % Versions.scalaReplPP,
  "org.json4s"           %% "json4s-native"     % Versions.json4s,
  "org.json4s" %% "json4s-jackson" % Versions.json4s,
  "org.apache.commons"    % "commons-text"      % Versions.commonsText,
  "org.scalatest"        %% "scalatest"         % Versions.scalatest % Test
)

Compile / doc / scalacOptions ++= Seq("-doc-title", "semanticcpg apidocs", "-doc-version", version.value)

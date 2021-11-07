name := "ghidra2cpg"

dependsOn(Projects.dataflowengineoss)

libraryDependencies ++= Seq(
  "com.github.scopt" %% "scopt"                    % "3.7.1",
  "commons-io"        % "commons-io"               % "2.7",
  "io.shiftleft"      % "ghidra"                   % "10.0_PUBLIC_20210621",
  "io.shiftleft"     %% "codepropertygraph"        % Versions.cpg,
  "io.shiftleft"     %% "codepropertygraph-protos" % Versions.cpg,
  "io.shiftleft"     %% "semanticcpg"              % Versions.cpg,
  "org.scalatest" %% "scalatest" % Versions.scalatest % Test,
  "io.shiftleft" %% "semanticcpg" % Versions.cpg % Test classifier "tests",
)

enablePlugins(JavaAppPackaging)

fork := true
javaOptions := Seq("-Djava.protocol.handler.pkgs=ghidra.framework.protocol")

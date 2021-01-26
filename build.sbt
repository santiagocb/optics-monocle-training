name := "optics-monocle-training"

version := "0.1"

scalaVersion := "2.13.4"

val monocleVersion = "2.0.4"

libraryDependencies ++= Seq(
  "com.github.julien-truffaut"  %%  "monocle-core"  % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-macro" % monocleVersion,
  "com.github.julien-truffaut"  %%  "monocle-law"   % monocleVersion  %   "test",
  "org.scalatest"               %% "scalatest"      % "3.0.8"         %   Test
)
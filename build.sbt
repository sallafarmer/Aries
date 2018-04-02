name := "Aries"

version := "0.1"

scalaVersion := "2.11.12"


scalacOptions ++= Seq(
  "-deprecation",
  "-unchecked",
  "-Yinline-warnings",
  "-language:implicitConversions",
  "-language:reflectiveCalls",
  "-language:higherKinds",
  "-language:postfixOps",
  "-language:existentials")


libraryDependencies ++= Seq(
  "org.locationtech.geotrellis" %% "geotrellis-spark" % "1.2.0",
  "org.locationtech.geotrellis" %% "geotrellis-s3" % "1.2.0",
  "org.locationtech.geotrellis" %% "geotrellis-vector" % "1.2.0",
  "org.locationtech.geotrellis" %% "geotrellis-raster" % "1.2.0",
  "org.locationtech.geotrellis" %% "geotrellis-proj4" % "1.2.0",

  "org.apache.spark"      %% "spark-core"       % "2.2.1" exclude("org.slf4j","slf4j-log4j12"),
  "org.apache.spark"      %% "spark-sql"       % "2.2.1" exclude("org.slf4j","slf4j-log4j12"),
  "org.apache.spark"      %% "spark-streaming"       % "2.2.1" exclude("org.slf4j","slf4j-log4j12"),

  "com.microsoft.azure" % "azure-storage" % "6.1.0" exclude("org.slf4j","slf4j-log4j12"),
  "org.apache.hadoop" % "hadoop-azure" % "3.0.0" exclude("org.slf4j","slf4j-log4j12"),

  "org.scalanlp" %% "breeze" % "1.0-RC2",

  "org.scalatest"         %%  "scalatest"       % "2.2.0" % Test,

  "ch.qos.logback" % "logback-classic" % "1.2.3",
  "org.slf4j" % "log4j-over-slf4j" % "1.7.25",
  "org.slf4j" % "slf4j-log4j12" % "1.7.25" % "provided",

  "black.ninia" % "jep" % "3.7.1"
)

dependencyOverrides += "com.fasterxml.jackson.core" % "jackson-core" % "2.8.7"
dependencyOverrides += "com.fasterxml.jackson.core" % "jackson-databind" % "2.8.7"
dependencyOverrides += "com.fasterxml.jackson.module" % "jackson-module-scala_2.11" % "2.8.7"

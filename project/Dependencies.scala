import sbt._

object Dependencies {

  import Versions._

  lazy val catsEffect     = "org.typelevel"    %% "cats-effect"     % catsEffectVersion
  lazy val catsRetry      = "com.github.cb372" %% "cats-retry"      % catsRetryVersion
  lazy val catsRetryMtl   = "com.github.cb372" %% "cats-retry-mtl"  % catsRetryVersion
  lazy val alleycatsRetry = "com.github.cb372" %% "alleycats-retry" % catsRetryVersion
  lazy val slf4jApi       = "org.slf4j"         % "slf4j-api"       % slf4jVersion
  lazy val slf4jSimple    = "org.slf4j"         % "slf4j-simple"    % slf4jVersion
  lazy val munit          = "org.scalameta"    %% "munit"           % munitVersion
  // lazy val newtype     = "io.estatico"                   %% "newtype"        % newTypeVersion

  // https://github.com/typelevel/kind-projector
  lazy val kindProjectorPlugin    = compilerPlugin(
    compilerPlugin("org.typelevel" % "kind-projector" % kindProjectorVersion cross CrossVersion.full)
  )
  // https://github.com/oleg-py/better-monadic-for
  lazy val betterMonadicForPlugin = compilerPlugin(
    compilerPlugin("com.olegpy" %% "better-monadic-for" % betterMonadicForVersion)
  )

  val compilerDependencies = Seq(
    catsEffect,
    catsRetry,
    catsRetryMtl,
    alleycatsRetry,
    slf4jApi,
    slf4jSimple,
    munit
  )

  val testDependencies = Seq.empty

  val pluginDependencies = Seq(kindProjectorPlugin, betterMonadicForPlugin)

  val allDependencies = compilerDependencies ++ testDependencies ++ pluginDependencies
}

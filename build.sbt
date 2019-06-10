lazy val root = project
  .in(file("."))
  .settings(
    organization := "tw.lanyitin",
    name := "common-ast",
    version := "0.1.0",
    isSnapshot := true,

    // To make the default compiler and REPL use Dotty
    scalaVersion := "2.12.7",
  )
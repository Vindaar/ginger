# Package

version       = "0.2.12"
author        = "Vindaar"
description   = "A Grid (R) like package in Nim"
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 1.0.0"
requires "chroma >= 0.1.0"
requires "https://github.com/Vindaar/seqmath >= 0.1.7"
requires "cairo >= 1.1.1"
requires "https://github.com/Vindaar/LatexDSL >= 0.1.4"
requires "shell"

task test, "Run tests":
  exec "nim c -r tests/test1.nim"

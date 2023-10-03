# Package

version       = "0.5.2"
author        = "Vindaar"
description   = "A Grid (R) like package in Nim"
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 1.0.0"
requires "chroma >= 0.1.0"
requires "https://github.com/Vindaar/seqmath >= 0.1.7"
requires "cairo >= 1.1.1"
requires "LatexDSL >= 0.1.13"
requires "pixie >= 4.4.0"
requires "shell"
requires "https://github.com/zetashift/fontim >= 0.2.0"

task test, "Run tests":
  exec "nim c -r tests/test1.nim"

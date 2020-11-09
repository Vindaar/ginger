# Package

version       = "0.2.8"
author        = "Vindaar"
description   = "A Grid (R) like package in Nim"
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 1.0.0"
requires "chroma >= 0.1.0"
requires "https://github.com/vindaar/seqmath >= 0.1.7"
requires "cairo >= 1.1.1"

task test, "Run tests":
  exec "nim c -r tests/test1.nim"

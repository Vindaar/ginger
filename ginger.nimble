# Package

version       = "0.1.2"
author        = "Vindaar"
description   = "A Grid (R) like package in Nim"
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 1.0.0"
requires "chroma#head"
requires "https://github.com/vindaar/seqmath >= 0.1.3"
requires "cairo"

task test, "Run tests":
  exec "nim c -r tests/test1.nim"

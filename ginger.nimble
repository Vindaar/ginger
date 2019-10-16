# Package

version       = "0.1.0"
author        = "Vindaar"
description   = "A Grid (R) like package in Nim"
license       = "MIT"
srcDir        = "src"


# Dependencies

requires "nim >= 0.19.9"
# requires "https://github.com/Vindaar/chroma#addMoreSpaces"
requires "https://github.com/vindaar/seqmath#head"
requires "cairo"

task test, "Run tests":
  exec "nim c -r tests/test1.nim"

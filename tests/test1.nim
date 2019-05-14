# This is just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest

import ginger
test "Coordinate conversions":
  # some simple tests for `toRelative` coord trafo
  let
    c1Rel = Coord(x: Coord1D(pos: 0.5, kind: ckRelative),
                  y: Coord1D(pos: 0.2, kind: ckRelative),
                  kind: ckRelative)
    c1Abs = Coord(x: (Coord1D(pos: 300, length: 600, kind: ckAbsolute)),
                  y: (Coord1D(pos: 100, length: 400, kind: ckAbsolute)),
                  kind: ckAbsolute)
    c2Rel = Coord(x: Coord1D(pos: 0.5, kind: ckRelative),
                  y: Coord1D(pos: 0.25, kind:ckRelative),
                  kind: ckRelative)
    c1Dat = Coord(x: (Coord1D(pos: 300, scale: (low: 100.0, high: 500.0), kind: ckData)),
                  y: (Coord1D(pos: 100, scale: (low: 0.0, high: 500.0), kind: ckData)),
                  kind: ckData)
    c2Dat = Coord(x: (Coord1D(pos: 300, scale: (low: -100.0, high: 300.0), kind: ckData)),
                  y: (Coord1D(pos: 100, scale: (low: -200.0, high: 200.0), kind: ckData)),
                  kind: ckData)
    c3Rel = Coord(x: Coord1D(pos: 1.0, kind: ckRelative),
                  y: Coord1D(pos: 0.75, kind:ckRelative),
                  kind: ckRelative)
  doAssert c1Rel.toRelative == c1Rel
  doAssert c1Abs.toRelative == c2Rel
  doAssert c1Dat.toRelative == c1Rel
  doAssert c2Dat.toRelative == c3Rel
  doAssert c2Rel.to(
    ckAbsolute,
    absWidth = some(600.0),
    absHeight = some(500.0)
  ) == c1Abs
  doAssert c1Abs.to(
    ckData,
    datxScale = some((low: 100.0, high: 500.0)),
    datyScale = some((low: 0.0, high: 400.0))
  ) == c2Rel
  doAssert c1Abs.to(
    ckData,
    datxScale = some((low: 100.0, high: 500.0)),
    datyScale = some((low: 0.0, high: 400.0)))
  .to(
    ckAbsolute,
    absWidth = some(c1Abs.x.length),
    absHeight = some(c1Abs.y.length)
  ) == c1Abs

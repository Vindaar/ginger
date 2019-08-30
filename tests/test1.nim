# This just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest

import ginger
#suite "Coordinate transformations":
#  test "Simple coordinate equalities":
#    # some simple tests for `toRelative` coord trafo
#    let
#      c1Rel = Coord(x: Coord1D(pos: 0.5, kind: ukRelative),
#                    y: Coord1D(pos: 0.2, kind: ukRelative))
#      c1Abs = Coord(x: (Coord1D(pos: 300, length: some(600.0), kind: ukAbsolute)),
#                    y: (Coord1D(pos: 100, length: some(400.0), kind: ukAbsolute)))
#      c2Rel = Coord(x: Coord1D(pos: 0.5, kind: ukRelative),
#                    y: Coord1D(pos: 0.25, kind:ukRelative))
#      c1Dat = Coord(x: (Coord1D(pos: 300, scale: (low: 100.0, high: 500.0), kind: ukData)),
#                    y: (Coord1D(pos: 100, scale: (low: 0.0, high: 500.0), kind: ukData)))
#      c2Dat = Coord(x: (Coord1D(pos: 300, scale: (low: -100.0, high: 300.0), kind: ukData)),
#                    y: (Coord1D(pos: 100, scale: (low: -200.0, high: 200.0), kind: ukData)))
#      c3Rel = Coord(x: Coord1D(pos: 1.0, kind: ukRelative),
#                    y: Coord1D(pos: 0.75, kind:ukRelative))
#    check c1Rel.toRelative == c1Rel
#    check c1Abs.toRelative == c2Rel
#    check c1Dat.toRelative == c1Rel
#    check c2Dat.toRelative == c3Rel
#
#    check c2Rel.to(
#      ukAbsolute,
#      absWidth = some(600.0),
#      absHeight = some(400.0)
#    ) == c1Abs
#    check c1Abs.to(
#      ukData,
#      datxScale = some((low: 100.0, high: 500.0)),
#      datyScale = some((low: 0.0, high: 400.0))
#    ) == c2Rel
#    check c1Abs.to(
#      ukData,
#      datxScale = some((low: 100.0, high: 500.0)),
#      datyScale = some((low: 0.0, high: 400.0)))
#    .to(
#      ukAbsolute,
#      absWidth = c1Abs.x.length,
#      absHeight =c1Abs.y.length
#    ) == c1Abs
#
#  test "Unit conversions":
#    let
#      c1cm = initCoord1D(2.54, ukCentimeter)
#      c1in = initCoord1D(1.0, ukInch)
#      c1pt = initCoord1D(72.27, ukAbsolute)
#    check c1cm == c1in
#    check c1cm == c1pt
#    check c1in == c1pt
#
#  test "Unit to relative conversions":
#    let
#      c1cm = initCoord1D(1.0, ukCentimeter)
#      c1in = initCoord1D(1.0, ukInch)
#      c1pt = initCoord1D(1.0, ukAbsolute)
#      # floating point error due to conversion
#      c1Rel = initCoord1D(0.09999999999999999)
#    check c1cm.toRelative(length = some(722.7 / 2.54)) == c1Rel
#    check c1in.toRelative(length = some(722.7)) == c1Rel

suite "Viewport":
  test "Mutable children":
    var view = initViewport()
    view.layout(2, 2)
    proc modChild(v: var Viewport, names: seq[string]) =
      v[0].name = names[0]
      v[1].name = names[1]
    view.modChild(@["first", "second"])
    check view[0].name == "first"
    check view[1].name == "second"

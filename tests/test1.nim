# This just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest
import sequtils

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

  test "ticks data scale update":
    let x = toSeq(0 .. 958).mapIt(it.float)
    let y = x.mapIt(it.float * it.float)
    let points = zip(x, y)
    var gobjPoints: seq[GraphObject]
    var view = initViewport()
    let xScale = (low: 0.0, high: x.max)
    let yScale = (low: 0.0, high: y.max)
    var view2 = initViewport(left = 0.25,
                             bottom = 0.5,
                             width = 0.75,
                             height = 0.5,
                             xScale = some(xScale),
                             yScale = some(yScale))
    for p in points:
      gobjPoints.add initPoint(view2, (x: p.a, y: p.b),
                               marker = mkCross)
    view2.addObj gobjPoints
    for p in view2.objects:
      check p.ptPos.x.kind == ukData
      check p.ptPos.y.kind == ukData
      check p.ptPos.x.scale == xScale
      check p.ptPos.y.scale == yScale
    let xticks = view2.xticks()
    let yticks = view2.yticks()
    for p in view2.objects:
      check p.ptPos.x.kind == ukData
      check p.ptPos.y.kind == ukData
      check p.ptPos.x.scale == (low: 0.0, high: 1000.0)
      check p.ptPos.y.scale == (low: 0.0, high: 1000000.0)

  test "ticks no data scale update":
    let x = toSeq(0 .. 958).mapIt(it.float)
    let y = x.mapIt(it.float * it.float)
    let points = zip(x, y)
    var gobjPoints: seq[GraphObject]
    var view = initViewport()
    let xScale = (low: 0.0, high: x.max)
    let yScale = (low: 0.0, high: y.max)
    var view2 = initViewport(left = 0.25,
                             bottom = 0.5,
                             width = 0.75,
                             height = 0.5,
                             xScale = some(xScale),
                             yScale = some(yScale))
    for p in points:
      gobjPoints.add initPoint(view2, (x: p.a, y: p.b),
                               marker = mkCross)
    view2.addObj gobjPoints
    for p in view2.objects:
      check p.ptPos.x.kind == ukData
      check p.ptPos.y.kind == ukData
      check p.ptPos.x.scale == xScale
      check p.ptPos.y.scale == yScale
    let xticks = view2.xticks(updateScale = false)
    let yticks = view2.yticks(updateScale = false)
    for p in view2.objects:
      check p.ptPos.x.kind == ukData
      check p.ptPos.y.kind == ukData
      check p.ptPos.x.scale == xScale
      check p.ptPos.y.scale == yScale

  test "ticks only data scale update once":
    let x = toSeq(0 .. 958).mapIt(it.float)
    let y = x.mapIt(it.float * it.float)
    let points = zip(x, y)
    var gobjPoints: seq[GraphObject]
    var view = initViewport()
    let xScale = (low: 0.0, high: x.max)
    let yScale = (low: 0.0, high: y.max)
    var view2 = initViewport(left = 0.25,
                             bottom = 0.5,
                             width = 0.75,
                             height = 0.5,
                             xScale = some(xScale),
                             yScale = some(yScale))
    for p in points:
      gobjPoints.add initPoint(view2, (x: p.a, y: p.b),
                               marker = mkCross)
    view2.addObj gobjPoints
    for p in view2.objects:
      check p.ptPos.x.kind == ukData
      check p.ptPos.y.kind == ukData
      check p.ptPos.x.scale == xScale
      check p.ptPos.y.scale == yScale
    let xticks = view2.xticks()
    let yticks = view2.yticks(updateScale = false)
    for p in view2.objects:
      check p.ptPos.x.kind == ukData
      check p.ptPos.y.kind == ukData
      check p.ptPos.x.scale == (low: 0.0, high: 1000.0)
      check p.ptPos.y.scale == yScale

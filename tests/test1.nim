# This just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest
import sequtils, math

import ginger


func almostEqual(c1, c2: Coord1D, eps = 1e-5): bool =
  result = abs(c1.pos - c2.pos) < eps

func almostEqual(c1, c2: Quantity, eps = 1e-5): bool =
  result = abs(c1.val - c2.val) < eps

func almostEqual(c1, c2: Coord, eps = 1e-5): bool =
  result = abs(c1.x.pos - c2.x.pos) < eps and abs(c1.y.pos - c2.y.pos) < eps

suite "Coordinate transformations":
  test "Simple coordinate equalities":
    # some simple tests for `toRelative` coord trafo
    let
      c1Rel = Coord(x: Coord1D(pos: 0.5, kind: ukRelative),
                    y: Coord1D(pos: 0.2, kind: ukRelative))
      c1Abs = Coord(x: Coord1D(pos: 300, length: some(quant(600.0, ukPoint)), kind: ukPoint),
                    y: Coord1D(pos: 100, length: some(quant(400.0, ukPoint)), kind: ukPoint))
      c2Rel = Coord(x: Coord1D(pos: 0.5, kind: ukRelative),
                    y: Coord1D(pos: 0.25, kind:ukRelative))
      c1Dat = Coord(x: (Coord1D(pos: 300, scale: (low: 100.0, high: 500.0), kind: ukData)),
                    y: (Coord1D(pos: 100, scale: (low: 0.0, high: 500.0), kind: ukData)))
      c2Dat = Coord(x: (Coord1D(pos: 300, scale: (low: -100.0, high: 300.0), kind: ukData)),
                    y: (Coord1D(pos: 100, scale: (low: -200.0, high: 200.0), kind: ukData)))
      c3Rel = Coord(x: Coord1D(pos: 1.0, kind: ukRelative),
                    y: Coord1D(pos: 0.75, kind:ukRelative))
    check almostEqual(c1Rel.toRelative, c1Rel)
    check almostEqual(c1Abs.toRelative, c2Rel)
    check almostEqual(c1Dat.toRelative, c1Rel)
    check almostEqual(c2Dat.toRelative, c3Rel)

    check c2Rel.to(
      ukPoint,
      absWidth = some(quant(600.0, ukPoint)),
      absHeight = some(quant(400.0, ukPoint))
    ) == c1Abs
    check c1Abs.to(
      ukData,
      datxScale = some((low: 100.0, high: 500.0)),
      datyScale = some((low: 0.0, high: 400.0))
    ) == c2Rel
    check c1Abs.to(
      ukData,
      datxScale = some((low: 100.0, high: 500.0)),
      datyScale = some((low: 0.0, high: 400.0)))
    .to(
      ukPoint,
      absWidth = c1Abs.x.length,
      absHeight =c1Abs.y.length
    ) == c1Abs

  test "Quantity comparisons":
    let
      qcm = quant(2.54, ukCentimeter)
      qin = quant(1.0, ukInch)
      qpt = quant(72.27, ukPoint)
    check almostEqual(qcm, qin.toCentimeter)
    check almostEqual(qcm, qpt.toCentimeter)
    check almostEqual(qin, qpt.toInch)

  test "Unit to relative conversions":
    let
      c1cm = initCoord1D(1.0, ukCentimeter)
      c1in = initCoord1D(1.0, ukInch)
      c1pt = initCoord1D(1.0, ukPoint)
      # floating point error due to conversion
      c1Rel = initCoord1D(0.09999999999999999)
    check almostEqual(c1cm.toRelative(length = some(quant(722.7 / 2.54, ukPoint))), c1Rel)
    check almostEqual(c1in.toRelative(length = some(quant(722.7, ukPoint))), c1Rel)

suite "Embeddings":
  test "Dummy":
    # TODO: have to write a lot more test, which test coordinatees under embeddings!
    # As far as I'm aware there are still some bugs lurking here :/
    discard

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

  test "Axes and labels":
    var view = initViewport()
    let x = toSeq(0 .. 958).mapIt(it.float)
    let y = x.mapIt(it.float * it.float)
    let xScale = (low: 0.0, high: x.max)
    let yScale = (low: 0.0, high: y.max)
    var child = initViewport(left = 0.15,
                             bottom = 0.4,
                             width = 0.75,
                             height = 0.5,
                             xScale = some(xScale),
                             yScale = some(yScale))
    var oldChild = child
    block:
      # x ticks and labels
      var mch = oldChild
      let xTicks = child.xticks()
      let xLabel = child.xlabel("X label")
      mch.addObj concat(xticks, @[xlabel])
      child.addObj concat(xticks, @[xlabel])
      for ch in mch.objects:
        case ch.kind:
        of goTick:
          check ch.tkAxis == akX
          check ch.tkPos.y == XAxisYPos()
        of goLabel:
          check ch.txtText == "X label"
          check ch.txtPos.y.pos.round.int == 283
        else: check false

    block:
      # x ticks and labels
      var mch = oldChild
      let yTicks = child.yticks()
      let yLabel = child.ylabel("Y label")
      mch.addObj concat(yticks, @[ylabel])
      child.addObj concat(yticks, @[ylabel])
      for ch in mch.objects:
        case ch.kind:
        of goTick:
          check ch.tkAxis == akY
          check ch.tkPos.x == YAxisXPos()
        of goLabel:
          check ch.txtText == "Y label"
          check ch.txtPos.x.pos.round.int == -43
        else: check false
    block:
      # x ticks and labels
      var mch = oldChild
      let xTicks = child.xticks(isSecondary = true)
      let xLabel = child.xlabel("X label sec", isSecondary = true)
      mch.addObj concat(xticks, @[xlabel])
      child.addObj concat(xticks, @[xlabel])
      for ch in mch.objects:
        case ch.kind:
        of goTick:
          check ch.tkAxis == akX
          check ch.tkPos.y == XAxisYPos(isSecondary = true)
          check ch.tkSecondary == true
        of goLabel:
          check ch.txtText == "X label sec"
          check ch.txtPos.y.pos.round.int == -43
        else: check false
    block:
      # x ticks and labels
      var mch = oldChild
      let yTicks = child.yticks(isSecondary =  true)
      let yLabel = child.ylabel("Y label sec", isSecondary = true)
      mch.addObj concat(yticks, @[ylabel])
      child.addObj concat(yticks, @[ylabel])
      for ch in mch.objects:
        case ch.kind:
        of goTick:
          check ch.tkAxis == akY
          check ch.tkPos.x == YAxisXPos(isSecondary = true)
          check ch.tkSecondary == true
        of goLabel:
          check ch.txtText == "Y label sec"
          check ch.txtPos.x.pos.round.int == 523
        else: check false
    view.children.add child
    view.draw("testAxes.pdf")

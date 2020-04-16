# This just an example to get you started. You may wish to put all of your
# tests into a single file, or separate them into multiple `test1`, `test2`
# etc. files (better names are recommended, just make sure the name starts with
# the letter 't').
#
# To run these tests, simply execute `nimble test`.

import unittest
import sequtils, math, seqmath

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

  test "Math with ukStrWidth":
    # Adding to a ukStrWidth can be useful
    let c1 = Coord1D(pos: 1.0, kind: ukStrWidth,
                     text: "Test text", font: Font(family: "sans-serif",
                                                   size: 16.0,
                                                   color: black))
    let c2 = Coord1D(pos: 0.3, kind: ukCentimeter)
    let c3 = c1 + c2
    check c3.kind == ukPoint
    check abs(c3.pos - 77.5358267) < 1e-4

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
      gobjPoints.add initPoint(view2, (x: p[0], y: p[0]),
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
      gobjPoints.add initPoint(view2, (x: p[0], y: p[0]),
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
      gobjPoints.add initPoint(view2, (x: p[0], y: p[0]),
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
    var oldChild: Viewport
    when defined(gcDestructors):
      oldChild[] = child[]
    else:
      oldChild = deepCopy(child)
    block:
      # x ticks and labels
      var mch: Viewport
      when defined(gcDestructors):
        mch[] = oldChild[]
      else:
        mch = deepCopy(oldChild)
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
      # rotated tick labels and labels, X axis
      var mch: Viewport
      when defined(gcDestructors):
        mch[] = oldChild[]
      else:
        mch = deepCopy(oldChild)
      let ticksLabs = block:
                    var locsC: seq[Coord1D]
                    let locs = linspace(0.1, 0.9, 5)
                    for i in 0 ..< 5:
                      locsC.add Coord1D(pos: locs[i], kind: ukRelative)
                    (locsC, locs.mapIt($it))
      let (tickObjs, labObjs) = child.tickLabels(ticksLabs[0], ticksLabs[1], akX, rotate = some(-45.0),
                                                 alignToOverride = some(taRight))
      # rotating the label is probably not that useful most of the time
      let xLabel = child.xlabel("X label", rotate = some(-30.0))
      mch.addObj concat(tickObjs, labObjs, @[xlabel])
      for ch in mch.objects:
        case ch.kind:
        of goTick:
          check ch.tkAxis == akX
          check ch.tkPos.y == XAxisYPos()
        of goLabel:
          check ch.txtText == "X label"
          check ch.rotate.isSome
          check ch.rotate.get == -30.0
        of goTickLabel:
          check ch.rotate.isSome
          check ch.rotate.get == -45.0
          check ch.txtAlign == taRight
        else: check false

    block:
      # y ticks and labels
      var mch: Viewport
      when defined(gcDestructors):
        mch[] = oldChild[]
      else:
        mch = deepCopy(oldChild)
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
      # rotated tick labels and labels, Y axis
      var mch: Viewport
      when defined(gcDestructors):
        mch[] = oldChild[]
      else:
        mch = deepCopy(oldChild)
      let ticksLabs = block:
                    var locsC: seq[Coord1D]
                    let locs = linspace(0.1, 0.9, 5)
                    for i in 0 ..< 5:
                      locsC.add Coord1D(pos: locs[i], kind: ukRelative)
                    (locsC, locs.mapIt($it))
      let (tickObjs, labObjs) = child.tickLabels(ticksLabs[0], ticksLabs[1], akY, rotate = some(-45.0),
                                                 alignToOverride = some(taRight))
      # rotating the label is probably not that useful most of the time
      let yLabel = child.xlabel("Y label", rotate = some(-30.0))
      mch.addObj concat(tickObjs, labObjs, @[ylabel])
      for ch in mch.objects:
        case ch.kind:
        of goTick:
          check ch.tkAxis == akY
          check ch.tkPos.x == YAxisXPos()
        of goLabel:
          check ch.txtText == "Y label"
          check ch.rotate.isSome
          check ch.rotate.get == -30.0
        of goTickLabel:
          check ch.rotate.isSome
          check ch.rotate.get == -45.0
          check ch.txtAlign == taRight
        else: check false

    block:
      # x ticks and labels, secondary
      var mch: Viewport
      when defined(gcDestructors):
        mch[] = oldChild[]
      else:
        mch = deepCopy(oldChild)
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
      # y ticks and labels
      var mch: Viewport
      when defined(gcDestructors):
        mch[] = oldChild[]
      else:
        mch = deepCopy(oldChild)
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

  test "Format tick value - zero is \"0\"":
    # bug: https://github.com/Vindaar/ggplotnim/issues/3
    # For certain (typically symmetric) scales, we may end up
    # with zero tick values calculated from e.g. `linspace`, which
    # were not the start value, e.g.
    let pos = linspace(-1.0, 1.0, 11)
    #[
      in this case the 0 tick position pos[5] will be `O(1e-17)`, due to
      floating point inaccuracies.
      We detect this in `formatTickValue` by comparing with the "scale" of
      the axis, that is the difference between two ticks. We use a heuristic
      rule to determine if the given tick value is the zero value or not,
      namely we divide by 10. At least from our automatic tick calculations
      we will never end up with ticks that go -0.1005, 0.005, 0.0995, where this
      would fail, due to 0.1 / 10 = 0.01 > 0.005
      If a user sets custom tick labels this is a non issue; formatTickValue will
      never be called
    ]#
    let expected = ["-1", "-0.8", "-0.6", "-0.4", "-0.2", "0", "0.2",
                    "0.4", "0.6", "0.8", "1"]
    for i in 0 .. 10:
      check expected[i] == formatTickValue(pos[i], 0.2) # `0.2` is the tick difference
    # on the other hand very small values should still produce the desired result
    let pos2 = linspace(-1e-12, 1e-12, 11)
    let expected2 = @["-1e-12", "-8e-13", "-6e-13", "-4e-13", "-2e-13",
                      "0", "2e-13", "4e-13", "6e-13", "8e-13", "1e-12"]
    let diff2 = (pos2.max - pos2.min) / 10.0
    for i in 0 .. 10:
      check expected2[i] == formatTickValue(pos2[i], diff2) # `0.2` is the tick difference

  test "Embed two finished plots into new root viewport":
    var plt1 = initViewport(name = "root1", wImg = 640.0, hImg = 480.0)
    # generate some children of `plt1`
    plt1.layout(2, 2)
    let plt2 = initViewport(name = "root2", wImg = 640.0, hImg = 480.0)

    check plt1.wImg == quant(640.0, ukPoint)
    check plt1.hImg == quant(480.0, ukPoint)
    check plt2.wImg == quant(640.0, ukPoint)
    check plt2.hImg == quant(480.0, ukPoint)
    for ch in plt1:
      check ch.wImg == quant(640.0, ukPoint)
      check ch.hImg == quant(480.0, ukPoint)

    # create new root viewport
    var plt = initViewport(name = "newRoot", wImg = 640.0, hImg = 480.0 * 2.0)
    # layout of 2 rows
    plt.layout(1, rows = 2)

    # embed via `embedInto`
    # NOTE: Assigning via `[]=` does not work correctly, because the two
    # viewports do not have the same `wImg`, `hImg`!
    # We need to use a proc which updates the `wImg` and `hImg` of the children
    plt.embedAt(0, plt1)
    plt.embedAt(1, plt2)

    check plt[0].name == "root1"
    check plt[1].name == "root2"

    proc checkRecurse(view: var Viewport) =
      for ch in mitems(view):
        check ch.wImg == quant(640.0, ukPoint)
        check ch.hImg == quant(480.0 * 2.0, ukPoint)
        ch.checkRecurse()

    plt.checkRecurse()

  test "Arithmetic for `Coord1D` involving ukData":

    let q1 = quant(50.0, ukData)
    let scale = (low: 25.0, high: 125.0)

    # sanity check. Conversion to `ukRelative` for a quantity
    # is handlded correctly
    check q1.toRelative(scale = some(scale)) == quant(0.5, ukRelative)

    # now for math involving ukData coordinates. In this case the
    # subtraction should be
    let exp = quant(q1.val - scale.low, ukData).toRelative(scale = some(scale))

    let cd1 = Coord1D(kind: ukData,
                      pos: 50.0,
                      scale: scale,
                      axis: akX)
    let cd2 = Coord1D(kind: ukPoint,
                      pos: 10.0,
                      length: some(quant(100.0, ukPoint)))
    let cd3 = Coord1D(kind: ukPoint,
                      pos: 0.0,
                      length: some(quant(100.0, ukPoint)))

    let cd12 = cd1 - cd2
    check cd12.pos == 0.15
    check cd12.kind == ukRelative

    let cd13 = cd1 - cd3
    check cd13.pos == 0.25
    # this should be the same as `exp`
    check cd13.pos == exp.val
    check cd13.kind == ukRelative

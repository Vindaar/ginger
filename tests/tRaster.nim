import random, viridisRaw, math, ginger, sequtils
var
  xs = newSeq[float]()
  ys = newSeq[float]()
  zs = newSeq[float]()
for x in 0 ..< 28:
  for y in 0 ..< 28:
    xs.add x.float
    ys.add y.float
    zs.add rand(1.0)

template toU8(f: float): int = round(f * 256.0).int

var drawCb = proc(): seq[uint32] =
  result = newSeq[uint32](28 * 28)
  for idx in 0 ..< zs.len:
    var colorIdx = (255.0 * (zs[idx])).round.int
    colorIdx = min(255, colorIdx)
    let cVal = ViridisRaw[colorIdx]
    result[idx] = (255 shl 24 or cVal[0].toU8 shl 16 or cVal[1].toU8 shl 8 or cVal[2].toU8).uint32

var img = initViewport()

var view1 = img.addViewport(left = 0.1,
                            bottom = 0.1,
                            width = 0.8,
                            height = 0.8,
                            xScale = some((low: 0.0, high: 1.0)),
                            yScale = some((low: 0.0, high: 1.0)))
let
  xticks = view1.xticks()
  yticks = view1.yticks()
  xtickLabels = view1.tickLabels(xticks)
  ytickLabels = view1.tickLabels(yticks)

let xlabel = view1.xlabel("x")
let ylabel = view1.ylabel("y")

view1.background()

let raster = view1.initRaster(c(0.0, 0.0),
                              width = quant(1.0, ukRelative),
                              height = quant(1.0, ukRelative),
                              numX = 28, numY = 28,
                              drawCb = drawCb)

let grdlines = view1.initGridLines(some(xticks), some(yticks))
view1.addObj concat(xticks, yticks, xticklabels, yticklabels, @[xlabel, ylabel, grdLines, raster])
img.children.add view1
img.draw("tRaster.pdf")

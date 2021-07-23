import chroma
import math
import types
import options

import os, latexdsl, strformat

#[
Maybe we have to collect all colors in a table or seq and create a custom 'preamble' that
contains color definitons?
]#

proc toTikZCoord(img: BImage, p: Point, isLength: static bool = false): Point =
  let ratio = img.height.float / img.width.float
  when isLength:
    result = (x: p.x / img.width.float, y: p.y / img.height.float)
  else:
    result = (x: p.x / img.width.float, y: (img.height.float - p.y) / img.height.float)
  result.y = result.y * ratio

func toStr(img: BImage, p: Point, isLength: static bool = false): string =
  block:
    let pst = img.toTikZCoord(p, isLength = isLength)
    var tmp = &"({pst.x}\\textwidth, {pst.y}\\textwidth)"
    tmp

func toStrDirect(p: Point, isLength: static bool = false): string =
  block:
    var tmp = &"({p.x}\\textwidth, {p.y}\\textwidth)"
    tmp

proc toStr(alignKind: TextAlignKind): string =
  ## convert the text align kind to the correct TikZ notation.
  ##
  ## In TikZ the the node is placed `right`, `left` (etc.) of the coordinate, wherease
  ## in cairo it is aligned by the `left` / `right` edge. Thus, the inversion.
  case alignKind
  of taLeft: result = "right"
  of taCenter: result = "" # default
  of taRight: result = "left"

proc nodeProperties(img: BImage, at: Point, alignKind: TextAlignKind, rotate: Option[float]): string =
  result = alignKind.toStr
  var rot: string
  if rotate.isSome:
    rot = "rotate = " & $(-rotate.get) # rotation is opposite of cairo
    result = if result.len > 0: result & ", " & rot else: rot
  if result.len > 0:
    result = "[" & result & "]"
  echo result

template latexAdd(body: untyped): untyped {.dirty.} =
  let toAdd = latex:
    body
  img.data.add toAdd

proc defColor(name: string, c: Color): string =
  let color = &"{c.r}, {c.g}, {c.b}"
  result = latex:
    \definecolor{`name`}{rgb}{`color`}

proc colorStr(style: Style): string =
  result.add defColor("drawColor", style.color)
  result.add defColor("fillColor", style.fillColor)

func getLineStyle(lineType: LineType, lineWidth: float): string =
  template dash: untyped = lineWidth * 4.0
  template dashSpace: untyped = lineWidth * 5.0
  template dot: untyped = lineWidth / 2.0
  template dotSpace: untyped = lineWidth * 2.0
  template longDash: untyped = lineWidth * 8.0

  case lineType
  of ltDashed:
    result = latex:
      dash pattern = on $(dash())pt off $(dashSpace())pt
  of ltDotted:
    result = latex:
      dash pattern = on $(dot())pt off $(dotSpace())pt
  of ltDotDash:
    result = latex:
      dash pattern = on $(dot())pt off $(dotSpace()) on $(dash())pt off $(dotSpace())
  of ltLongDash:
    result = latex:
      dash pattern = on $(longDash())pt off $(dashSpace())
  of ltTwoDash:
    result = latex:
      dash pattern = on $(dash())pt off $(dotSpace() * 2.0) on $(longDash())pt off $(dotSpace() * 2.0)
  else: discard

proc lineStyle(style: Style, drawColor = "drawColor", fillColor = "fillColor"): string =
  let fillOp = &"fill opacity = {style.fillColor.a}"
  let colorOp = &"draw opacity = {style.color.a}"
  let lineDash = getLineStyle(style.lineType, style.lineWidth)
  result = &"[color = {drawColor}, fill = {fillColor}, {colorOp}, {fillOp}, line width = {style.lineWidth}pt"
  if lineDash.len > 0:
    result.add ", " & lineDash & "]"
  else:
    result.add "]"

proc drawLine*(img: var BImage, start, stop: Point,
               style: Style,
               rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  let p0 = img.toStr(start)
  let p1 = img.toStr(stop)
  let color = style.colorStr
  let lineSt = style.lineStyle
  latexAdd:
    `color`
    \draw " " `lineSt` " " `p0` " " -- " " `p1` ";"

proc drawPolyLine*(img: var BImage, points: seq[Point],
                   style: Style,
                   rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  let lineSt = style.lineStyle
  let color = style.colorStr
  echo lineSt
  latexAdd:
    `color`
    \draw " " `lineSt`
  for i, p in points:
    let pStr = img.toStr(p)
    if i == points.high:
      latexAdd:
        `pStr`
    else:
      latexAdd:
        `pStr` " -- "
  img.data.add ";"

proc drawCircle*(img: var BImage, center: Point, radius: float,
                 lineWidth: float,
                 strokeColor = color(0.0, 0.0, 0.0),
                 fillColor = color(0.0, 0.0, 0.0, 0.0),
                 rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  let p = img.toStr(center)
  let radius = $(radius / img.width.float) & "\\textwidth"# / img.width.float
  let style = Style(lineWidth: lineWidth,
                    color: strokeColor,
                    fillColor: fillColor)
  let color = style.colorStr
  let lineSt = style.lineStyle
  latexAdd:
    `color`
    \draw " " `lineSt` " " `p` " " circle " " [radius = `radius`] ";"

proc getTextExtent*(text: string, font: Font): TextExtent =
  ## XXX: HACK
  let ptY = font.size
  let ptX = ptY
  result = TextExtent(
    x_bearing: 0.0,
    y_bearing: 0.0,
    width: ptX * text.len.float,
    height: ptY)
  result.x_advance = result.width
  result.y_advance = result.height

proc drawText*(img: var BImage, text: string, font: Font, at: Point,
               alignKind: TextAlignKind = taLeft,
               rotate: Option[float] = none[float](),
               rotateInView: Option[(float, Point)] = none[(float, Point)]()) =
  ## TODO: fix this coord mess
  var
    x = at.x
    y = at.y

  let extents = getTextExtent("M", font)
  # potentially rotate around specific point (location depends on where we align)
  case alignKind
  of taLeft:
    x = at.x - (extents.width / 2.0 + extents.x_bearing)
  of taRight:
    x = at.x + (extents.width / 2.0 + extents.x_bearing)
  of taCenter:
    #x = at.x - (extents.width / 2.0 + extents.x_bearing)
    #y = at.y + (extents.height / 2.0 + extents.y_bearing)
    discard
  else: discard

  let at = (x: x, y: y)
  let atStr = img.toStr(at)
  let alignStr = img.nodeProperties(at, alignKind, rotate)
  echo "ext at ", atStr, " with ", alignStr, " of ", text, " from ", at
  let ts = $(font.size)
  let tst = $((font.size) * 1.2)
  var text = latex:
    \fontsize{`ts`}{`tst`}\selectfont " " `text`
  #if rotate.isSome:
  #  let rt = $(-rotate.get)
  #  text = latex:
  #    \rotatebox{`rt`}{`text`}
  latexAdd:
    \node " " `alignStr` " at " `atStr` {`text`} ";"

proc drawRectangle*(img: var BImage, left, bottom, width, height: float,
                    style: Style,
                    rotate: Option[float] = none[float](),
                    rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
  let atPt = img.toTikZCoord((x: left, y: bottom))
  let sizePt = img.toTikZCoord((x: left + width, y: bottom + height), isLength = false)
  if style.gradient.isSome:
    let gradient = style.gradient.get
    let heightRel = height / img.width.float
    let sliceHeight = heightRel / (gradient.colors.len.float - 1)
    var curBottom = atPt.y - heightRel
    for i, c in gradient.colors:
      let n = "color" & $i
      let atStr = (x: atPt.x, y: curBottom).toStrDirect # left bottom coords
      let sizeStr = (
        x: sizePt.x,
        y: curBottom + sliceHeight + 1e-3               # add some ε for overlap
      ).toStrDirect                                     # end coords
      let curColor = defColor(n, c)
      let curLineStyle = style.lineStyle(fillColor = n)
      latexAdd:
        `curColor`
        \draw " " `curLineStyle` " " `atStr` " " rectangle " " `sizeStr` ";"
      curBottom += sliceHeight
  else:
    let color = style.colorStr
    let lineSt = style.lineStyle
    let sizeStr = sizePt.toStrDirect
    let atStr = atPt.toStrDirect
    latexAdd:
      `color`
      \draw " " `lineSt` " " `atStr` " " rectangle " " `sizeStr` ";"

proc drawRaster*(img: var BImage, left, bottom, width, height: float,
                 numX, numY: int,
                 drawCb: proc(): seq[uint32],
                 rotate: Option[float] = none[float](),
                 rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
  debugecho "WARNING: `drawRaster` of TikZ backend is not implemented yet!"

proc initBImage*(filename: string,
                 width, height: int,
                 fType: FiletypeKind): BImage =

  result = BImage(fname: filename,
                  backend: bkTikZ,
                  width: width, height: height,
                  fType: fType)

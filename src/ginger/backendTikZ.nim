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
  ## convert the text align kind to the correct tikz notation
  case alignKind
  of taLeft: result = "right"
  of taCenter: result = "" # default
  of taRight: result = "left"

proc nodeProperties(img: BImage, at: Point, alignKind: TextAlignKind, rotate: Option[float]): string =
  result = "["
  result.add alignKind.toStr
  if rotate.isSome:
    #result.add ", rotate around = {" & $(-rotate.get) & ":" & at.toStr & "}" # rotation opposite of cairo
    result.add ", rotate = " & $(-rotate.get) # rotation opposite of cairo
  result.add "]"
  echo result

template latexAdd(body: untyped): untyped {.dirty.} =
  let toAdd = latex:
    body
  img.data.add toAdd

proc defColor(name: string, c: Color): string =
  let color = &"{c.r * 256.0}, {c.g * 256.0}, {c.b * 256.0}"
  result = latex:
    \definecolor{`name`}{RGB}{`color`}

proc colorStr(style: Style): string =
  result.add defColor("drawColor", style.color)
  result.add defColor("fillColor", style.fillColor)

proc lineStyle(style: Style, drawColor = "drawColor", fillColor = "fillColor"): string =
  #let color = "{" & &"{style.color.r * 256.0}, {style.color.g * 256.0}, {style.color.b * 256.0}" & "}"
  let fillOp = &"fill opacity = {style.fillColor.a}"
  let colorOp = &"draw opacity = {style.color.a}"
  result = &"[color = {drawColor}, fill = {fillColor}, {colorOp}, {fillOp}, line width = {style.lineWidth}pt]"

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
import chroma
import math
import types
import options

import os, latexdsl, strformat

#[
Maybe we have to collect all colors in a table or seq and create a custom 'preamble' that
contains color definitons?
]#

template toStr(p: Point): string {.dirty.} =
  block:
    let pst = (x: p.x / img.width.float, y: (img.height.float - p.y) / img.height.float)
    &"({pst.x}\\textwidth, {pst.y}\\textwidth)"

template latexAdd(body: untyped): untyped {.dirty.} =
  let toAdd = latex:
    body
  img.data.add toAdd

proc drawLine*(img: var BImage, start, stop: Point,
               style: Style,
               rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  let p0 = start.toStr
  let p1 = stop.toStr
  let colors = &"{style.color.r}, {style.color.g}, {style.color.b}" # , {style.color.a}
  let color = latex:
    \definecolor{foocolor}{RGB}{`colors`}
  let lineWidth = &"[line width = {style.lineWidth}pt, fill = foocolor]"
  #,fill={rgb:red,4;green,2;yellow,1}
  latexAdd:
    `color`
    \draw " " `lineWidth` " " `p0` " " -- " " `p1` ";"

proc drawPolyLine*(img: var BImage, points: seq[Point],
                   style: Style,
                   rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  latexAdd:
    \draw
  for i, p in points:
    let pStr = p.toStr
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
  let p = center.toStr
  latexAdd:
    \draw " " `p` " " circle " " [radius = `radius`] ";"

proc getTextExtent*(text: string, font: Font): TextExtent =
  ## XXX: HACK
  const pt = 20.0
  result = TextExtent(
    x_bearing: 0.0,
    y_bearing: 0.0,
    width: pt * text.len.float,
    height: pt)
  result.x_advance = result.width
  result.y_advance = result.height

proc drawText*(img: var BImage, text: string, font: Font, at: Point,
               alignKind: TextAlignKind = taLeft,
               rotate: Option[float] = none[float](),
               rotateInView: Option[(float, Point)] = none[(float, Point)]()) =
  let atStr = at.toStr
  latexAdd:
    \node " at " `atStr` {`text`} ";"

proc drawRectangle*(img: var BImage, left, bottom, width, height: float,
                    style: Style,
                    rotate: Option[float] = none[float](),
                    rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
  let atStr = (x: left, y: bottom).toStr
  let sizeStr = (x: width, y: height).toStr
  latexAdd:
    \draw " " `atStr` " " rectangle " " `sizeStr` ";"

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

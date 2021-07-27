import chroma
import math
import types
import options

proc drawLine*(img: BImage, start, stop: Point,
               style: Style,
               rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  debugecho "WARNING: `drawLine` of dummy backend is being called!"

proc drawPolyLine*(img: BImage, points: seq[Point],
                   style: Style,
                   rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  debugecho "WARNING: `drawPolyLine` of dummy backend is being called!"

proc drawCircle*(img: BImage, center: Point, radius: float,
                 lineWidth: float,
                 strokeColor = color(0.0, 0.0, 0.0),
                 fillColor = color(0.0, 0.0, 0.0, 0.0),
                 rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  debugecho "WARNING: `drawCircle` of dummy backend is being called!"

proc getTextExtent*(backend: BackendKind, text: string, font: Font): TextExtent =
  debugecho "WARNING: `getTextExtent` of dummy backend is being called!"

proc drawText*(img: BImage, text: string, font: Font, at: Point,
               alignKind: TextAlignKind = taLeft,
               rotate: Option[float] = none[float](),
               rotateInView: Option[(float, Point)] = none[(float, Point)]()) =
  debugecho "WARNING: `drawText` of dummy backend is being called!"

proc drawRectangle*(img: BImage, left, bottom, width, height: float,
                    style: Style,
                    rotate: Option[float] = none[float](),
                    rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
  debugecho "WARNING: `drawRectangle` of dummy backend is being called!"

proc drawRaster*(img: var BImage, left, bottom, width, height: float,
                 numX, numY: int,
                 drawCb: proc(): seq[uint32],
                 rotate: Option[float] = none[float](),
                 rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
  debugecho "WARNING: `drawRaster` of dummy backend is being called!"


proc initBImage*(filename: string,
                 width, height: int,
                 fType: FiletypeKind,
                 texOptions: TeXOptions): BImage =
  debugecho "WARNING: `initBImage` of dummy backend is being called!"

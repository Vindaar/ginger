import chroma
import math
import types
import options

proc drawLine*(img: BImage[DummyBackend], start, stop: Point,
               style: Style,
               rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  debugecho "WARNING: `drawLine` of dummy backend is being called!"

proc drawPolyLine*(img: BImage[DummyBackend], points: seq[Point],
                   style: Style,
                   rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  debugecho "WARNING: `drawPolyLine` of dummy backend is being called!"

proc drawCircle*(img: BImage[DummyBackend], center: Point, radius: float,
                 lineWidth: float,
                 strokeColor = color(0.0, 0.0, 0.0),
                 fillColor = color(0.0, 0.0, 0.0, 0.0),
                 rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  debugecho "WARNING: `drawCircle` of dummy backend is being called!"

proc getTextExtent*[T](_: typedesc[T], fType: FileTypeKind, text: string, font: Font): TextExtent =
  debugecho "WARNING: `getTextExtent` of dummy backend is being called!"

proc drawText*(img: BImage[DummyBackend], text: string, font: Font, at: Point,
               alignKind: TextAlignKind = taLeft,
               rotate: Option[float] = none[float](),
               rotateInView: Option[(float, Point)] = none[(float, Point)]()) =
  debugecho "WARNING: `drawText` of dummy backend is being called!"

proc drawRectangle*(img: BImage[DummyBackend], left, bottom, width, height: float,
                    style: Style,
                    rotate: Option[float] = none[float](),
                    rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
  debugecho "WARNING: `drawRectangle` of dummy backend is being called!"

proc drawRaster*(img: var BImage[DummyBackend], left, bottom, width, height: float,
                 numX, numY: int,
                 drawCb: proc(): seq[uint32],
                 rotate: Option[float] = none[float](),
                 rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
  debugecho "WARNING: `drawRaster` of dummy backend is being called!"

proc initBImage*(_: typedesc[DummyBackend],
                 filename: string,
                 width, height: int,
                 fType: FiletypeKind,
                 texOptions = TeXOptions()): BImage[DummyBackend] =
  debugecho "WARNING: `initBImage` of dummy backend is being called!"

proc destroy*(img: var BImage[DummyBackend]) =
  debugecho "WARNING: `destroy` of dummy backend is being called!"

proc insertRaster*(img: var BImage[DummyBackend], tmpName: string, left, bottom, width, height: float) =
  debugecho "WARNING: `insertRaster` of dummy backend is being called!"

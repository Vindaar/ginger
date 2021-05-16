import chroma
import options
import pixie
import types

proc toVec2(point: Point): Vec2 =
  # Helper to convert ginger's Points to vec2's for Pixie
  result = vec2(point.x, point.y)

proc rotate(path: var Path, angle: float, around: Point) =
  # We use the low-level Path API because it allows us rotation
  path.transform(translate(around.toVec2))
  path.transform(rotate((angle * PI / 180.0).float32))
  path.transform(translate(-around.toVec2))

proc drawLine*(img: BImage, start, stop: Point,
               style: Style,
               rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  var path: Path
  path.moveTo(start.toVec2)
  path.lineTo(stop.toVec2)
  if rotateAngle.isSome: 
    let (angle, around) = rotateAngle.get
    path.rotate(angle, around)
  img.pxImage.strokePath(path, style.color, style.lineWidth)
    
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

proc getTextExtent*(text: string, font: types.Font): TextExtent =
  debugecho "WARNING: `getTextExtent` of dummy backend is being called!"

proc drawText*(img: BImage, text: string, font: types.Font, at: Point,
               alignKind: TextAlignKind = taLeft,
               rotate: Option[float] = none[float](),
               rotateInView: Option[(float, Point)] = none[(float, Point)]()) =
  debugecho "WARNING: `drawText` of dummy backend is being called!"

proc drawRectangle*(img: BImage, left, bottom, width, height: float,
                    style: Style,
                    rotate: Option[float] = none[float](),
                    rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
  var path: Path
  let (x, y) = (left, left + height) # Pixie rects start at top-right
  path.rect(x, y, width, height)
  if rotateInView.isSome:
    let (angle, around) = rotateInView.get
    path.rotate(angle, around)
  if rotate.isSome:
    path.rotate(rotate.get, (left, bottom))
  img.pxImage.fillPath(path, style.color)

proc drawRaster*(img: var BImage, left, bottom, width, height: float,
                 numX, numY: int,
                 drawCb: proc(): seq[uint32],
                 rotate: Option[float] = none[float](),
                 rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
  debugecho "WARNING: `drawRaster` of dummy backend is being called!"

proc initBImage*(filename: string,
                 width, height: int,
                 backend: BackendKind,
                 fType: FiletypeKind): BImage =
  case backend
  of bkPixie:
    var image: Image = newImage(width, height)
    case fType
    of fkPng:
      image.writeFile(filename)
    else:
      raise newException(Exception, "Unsupported filetype " & $fType & " in `initBImage`")
    result = BImage(fname: filename,
                    width: width,
                    height: height,
                    backend: bkPixie,
                    pxImage: image,
                    filetype: fType)
  of bkCairo:
    discard
  of bkVega:
    discard

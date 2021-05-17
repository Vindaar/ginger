import chroma
import options
import pixie
import types

func toVec2(point: Point): Vec2 =
  # Helper to convert ginger's Points to vec2's for Pixie
  result = vec2(point.x, point.y)

func rotationMatrix(angle: float, around: Point): Mat3 =
  # Returns the transformation matrix enabling easy rotation around a point
  result = translate(around.toVec2) * rotate((angle * PI / 180.0).float32) * translate(-around.toVec2)

proc rotate(path: var Path, angle: float, around: Point) =
  # We use the low-level Path API because it's more flexible
  path.transform(rotationMatrix(angle, around))

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
  # Draws lines sequentially to every point in `points`
  var path: Path
  let startPoint = points[0].toVec2
  path.moveTo(startPoint)
  
  for idx in 1..points.high:
    path.lineTo(points[idx].toVec2)

  img.pxImage.strokePath(path, style.color, style.lineWidth)
  # When drawing that closes it will fill!
  img.pxImage.fillPath(path, style.fillColor)

proc drawCircle*(img: BImage, center: Point, radius: float,
                 lineWidth: float,
                 strokeColor = color(0.0, 0.0, 0.0),
                 fillColor = color(0.0, 0.0, 0.0, 0.0),
                 rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  var path: Path
  path.circle(center.toVec2, radius)

  if rotateAngle.isSome:
    let (angle, around) = rotateAngle.get
    path.rotate(angle, around)

  img.pxImage.fillPath(path, fillColor)
  img.pxImage.strokePath(path, strokeColor, lineWidth)

proc getTextExtent*(text: string, font: types.Font): TextExtent =
  debugecho "WARNING: `getTextExtent` of Pixie backend is being called and is unnessecary"

func align(alignKind: TextAlignKind): (HAlignMode, VAlignMode) =
  # Return Pixie alignments given a Ginger text alignment
  case alignKind:
  of taLeft:
    result = (haLeft, vaMiddle)
  of taCenter:
    result = (haCenter, vaMiddle)
  of taRight:
    result = (haRight, vaMiddle)

proc drawText*(img: BImage, text: string, font: types.Font, at: Point,
               alignKind: TextAlignKind = taLeft,
               rotate: Option[float] = none[float](),
               rotateInView: Option[(float, Point)] = none[(float, Point)]()) =
  var pxFont = readFont(font.family) # TODO: family AFAIK does not point to a valid font file so this will fail
  pxFont.size = font.size
  # pxFont.paint.color = font.color
  # TODO figure out font slant and Pixie

  var transformMatrix: Mat3 = mat3()
  # Rotate the text first and then align it
  if rotateInView.isSome:
    let (angle, around) = rotateInView.get
    transformMatrix = transformMatrix * rotationMatrix(angle, around)
  if rotate.isSome:
    transformMatrix = transformMatrix * rotationMatrix(rotate.get, (at.x, at.y))

  let (hAlign, vAlign) = align(alignKind)
  # Render the text to the image using our transformation matrix and alignment
  img.pxImage.fillText(pxFont, text, transformMatrix, vec2(0,0), hAlign, vAlign)

proc drawRectangle*(img: BImage, left, bottom, width, height: float,
                    style: Style,
                    rotate: Option[float] = none[float](),
                    rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
  var path: Path
  path.rect(bottom, left, width, height)

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
  debugecho "WARNING: `drawRaster` of Pixie backend is being called!"

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

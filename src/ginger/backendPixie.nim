import chroma
import std/[options, strformat]
import pixie
import types
import fontim

func toVec2(point: Point): Vec2 =
  # Helper to convert ginger's Points to vec2's for Pixie
  result = vec2(point.x, point.y)

proc saveState(img: BImage[PixieBackend]) =
  # Helper to save the current state of the Context
  # and reset to the identity matrix afterwards
  img.pxContext.save()
  img.pxContext.resetTransform()

proc rotate(img: BImage[PixieBackend], angle: float, around: Point) =
  # Adds the matrix for rotating the given Context of `img` with `angle` around a Point
  # to the Context
  img.pxContext.translate(around.toVec2)
  img.pxContext.rotate((angle * PI / 180.0).float32)
  img.pxContext.translate(-around.toVec2)

proc setStyle(img: BImage[PixieBackend], style: Style) =
  # Styles the Pixie Context according to a given Ginger `style`
  let
    fillPaint = Paint(kind: SolidPaint, color: style.fillColor)
    strokePaint = Paint(kind: SolidPaint, color: style.color)

  img.pxContext.fillStyle = fillPaint
  img.pxContext.strokeStyle = strokePaint
  img.pxContext.lineWidth = style.lineWidth

proc drawLine*(img: BImage[PixieBackend], start, stop: Point,
               style: Style,
               rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  # Draws a line from `start` to `stop`
  # First we check if we need to rotate the line around a point
  if rotateAngle.isSome:
    let (angle, around) = rotateAngle.get
    img.rotate(angle, around)

  img.setStyle(style)
  img.pxContext.strokeSegment(segment(start.toVec2, stop.toVec2))
  img.saveState()

proc drawPolyLine*(img: BImage[PixieBackend], points: seq[Point],
                   style: Style,
                   rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  # Draws lines sequentially to every point in `points`
  if rotateAngle.isSome:
    let (angle, around) = rotateAngle.get
    img.rotate(angle, around)

  var path: Path
  let startPoint = points[0].toVec2
  path.moveTo(startPoint)

  for idx in 1..points.high:
    path.lineTo(points[idx].toVec2)

  img.setStyle(style)
  img.pxContext.stroke(path)
  # When drawing a line that closes a path, it will fill it
  img.pxContext.fill(path)
  img.saveState()

proc drawCircle*(img: var BImage[PixieBackend], center: Point, radius: float,
                 lineWidth: float,
                 strokeColor = color(0.0, 0.0, 0.0),
                 fillColor = color(0.0, 0.0, 0.0, 0.0),
                 rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  # A temporary function for drawCircle where a style isn't passed a long
  if rotateAngle.isSome:
    let (angle, around) = rotateAngle.get
    img.rotate(angle, around)

  let
    fillPaint = Paint(kind: SolidPaint, color: fillColor)
    strokePaint = Paint(kind: SolidPaint, color: strokeColor)

  img.pxContext.fillStyle = fillPaint
  img.pxContext.strokeStyle = strokePaint
  img.pxContext.lineWidth = lineWidth
  img.pxContext.strokeCircle(circle(center.toVec2, radius))
  img.pxContext.fillCircle(circle(center.toVec2, radius))
  img.saveState()

proc drawCircle*(img: BImage[PixieBackend], center: Point, radius: float,
                 style: Style,
                 rotateAngle: Option[(float, Point)] = none[(float, Point)]()) =
  if rotateAngle.isSome:
    let (angle, around) = rotateAngle.get
    img.rotate(angle, around)

  img.setStyle(style)

  img.pxContext.strokeCircle(circle(center.toVec2, radius))
  img.pxContext.fillCircle(circle(center.toVec2, radius))
  img.saveState()

proc getTextExtent*(_: typedesc[PixieBackend], fType: FileTypeKind, text: string, font: types.Font): TextExtent =
  # This is for now copied from the TikZ backend until I figure out how to
  # use `computeBounds` from pixie.fonts
  let ptY = font.size
  let ptX = ptY * 0.5
  result = TextExtent(
    x_bearing: 0.0,
    y_bearing: 0.0,
    width: ptX * text.len.float,
    height: ptY,
  )
  result.x_advance = result.width
  result.y_advance = result.height

func getTextAligns(alignKind: TextAlignKind): HorizontalAlignment =
  # Return Pixie alignments given a Ginger text alignment
  result = case alignKind
    of taLeft: LeftAlign
    of taCenter: CenterAlign
    of taRight: RightAlign

func getSlant(fs: FontSlant): string =
  # Returns the suffix for a given slant
  result = case fs
    of fsNormal: "Regular"
    of fsItalic: "Italic"
    of fsOblique: "Oblique"

proc setFont(img: BImage[PixieBackend], font: types.Font) =
  # Find try to find path of the given font, else load a default font
  var pxFont: string
  let path = getFontPath(font.family, font.slant.getSlant)
  if path.isSome:
    pxFont = path.get
  else:
    # This can still fail because Arial could be named like ArialSans.
    pxFont = getFontPath("Arial Sans", "Regular").get

  img.pxContext.font = pxFont

proc drawText*(img: BImage[PixieBackend], text: string, font: types.Font, at: Point,
               alignKind: TextAlignKind = taLeft,
               rotate: Option[float] = none[float](),
               rotateInView: Option[(float, Point)] = none[(float, Point)](),
               fontPath: Option[string] = none[string]()) =
  img.setFont(font)

  # Check if we need to rotate around a specified Point on the canvas
  if rotateInView.isSome:
    let (angle, around) = rotateInView.get
    img.rotate(angle, around)
  if rotate.isSome:
    # Here we only rotate with an angle around the origin if needed
    img.rotate(rotate.get, (at.x, at.y))

  img.pxContext.fontSize = font.size
  img.pxContext.fillStyle = font.color
  img.pxContext.strokeStyle = font.color
  img.pxContext.textAlign = getTextAligns(alignKind)
  img.pxContext.fillText(text, at.toVec2)
  img.saveState()

proc drawRectangle*(img: BImage[PixieBackend], left, bottom, width, height: float,
                    style: Style,
                    rotate: Option[float] = none[float](),
                    rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
  # Check if we need to rotate around a specified Point on the canvas
  if rotateInView.isSome:
    let (angle, around) = rotateInView.get
    img.rotate(angle, around)
  if rotate.isSome:
    # Here we only rotate with an angle around the origin if needed
    img.rotate(rotate.get, (left, bottom))

  img.setStyle(style)
  let rect = rect(left, bottom, width, height)
  img.pxContext.strokeRect(rect)
  img.pxContext.fillRect(rect)
  img.saveState()

func toColorRGBA(x: uint32): ColorRGBA =
  # Converts uint32 to a Pixie compatible color for drawing a raster
  let alpha = x shr 24 # no `and` necessary, since higher bits now all 0
  let r = x shr 16 and 0xFF'u32
  let g = x shr 8 and 0xFF'u32
  let b = x and 0xFF'u32
  result = ColorRGBA(r: r.uint8, g: g.uint8, b: b.uint8, a: alpha.uint8)

proc drawRaster*(img: var BImage[PixieBackend], left, bottom, width, height: float,
                 numX, numY: int,
                 drawCb: proc(): seq[uint32],
                 rotate: Option[float] = none[float](),
                 rotateInView: Option[(float, Point),] = none[(float, Point)]()) =
  # Draws a raster for grid-like data, `drawCb` returns a 1D sequence
  # representing a 2D grid. Based on it's index we compute the (x, y) coordinate

  # Check if we need to rotate around a specified Point on the canvas
  if rotateInView.isSome:
    let (angle, around) = rotateInView.get
    img.rotate(angle, around)
  if rotate.isSome:
    # Here we only rotate with an angle around the origin if needed
    img.rotate(rotate.get, (left, bottom))

  # Initialize our raster
  let
    rasterWidth = abs(width).int32
    rasterHeight = abs(height).int32
    # Dimensions of a single tile in pixels
    tileWidth = width / numX.float
    tileHeight = height / numY.float
    toDraw = drawCb() # Get the sequence that holds the color information
  var raster = newImage(rasterWidth, rasterHeight)

  for y in 0 ..< rasterHeight:
    for x in 0 ..< rasterWidth:
      # Get the element in `toDraw` and apply the color on the corresponding pixel on our raster
      let tX = (x.float / tileWidth).floor.int
      let tY = (y.float / tileHeight).floor.int
      raster[x, y] = toDraw[tY * numX + tX].toColorRGBA

  # Actually draw the raster on to the Context
  img.pxContext.drawImage(raster, pos = vec2(left, bottom))
  img.saveState()

proc insertRaster*(img: var BImage[PixieBackend], tmpName: string, left, bottom, width, height: float) =
  raise newException(Defect, "`insertRaster` not implemenetd for Pixie backend yet.")

proc initBImage*(_: typedesc[PixieBackend],
                 filename: string,
                 width, height: int,
                 fType: FiletypeKind,
                 texOptions = TeXOptions()): BImage[PixieBackend] =
  var ctx = newContext(width, height)

  case fType
  of fkPng:
    ctx.image.writeFile(filename)
  else:
    raise newException(Exception, "Unsupported filetype " & $fType & " in `initBImage[PixieBackend]`")
  let backend = PixieBackend(pxContext: ctx)
  result = BImage[PixieBackend](fname: filename,
                                width: width,
                                height: height,
                                ftype: fType)

proc writeFile*(img: BImage[PixieBackend], fname: string) {.inline.} =
  # Helper that writes an Image using it's Context to a file
  img.pxContext.image.writeFile(fname)

# destroy is a no-op for Pixie
## XXX: why though? And why is there a `writeFile` call in the `init` procedure?
proc destroy*(img: var BImage[PixieBackend]) = discard
